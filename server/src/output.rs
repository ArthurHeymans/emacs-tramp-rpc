// SPDX-License-Identifier: GPL-3.0-or-later

//! Serialized framing and connection-wide output failure.

use std::io;
use tokio::io::{AsyncWrite, AsyncWriteExt};
use tokio::sync::{Mutex, watch};

pub struct FrameWriter<W> {
    writer: Mutex<W>,
    failed: watch::Sender<bool>,
}

impl<W> FrameWriter<W> {
    pub fn new(writer: W) -> Self {
        Self {
            writer: Mutex::new(writer),
            failed: watch::channel(false).0,
        }
    }

    pub fn failure(&self) -> watch::Receiver<bool> {
        self.failed.subscribe()
    }

    pub fn close(&self) {
        self.failed.send_replace(true);
    }
}

impl<W: AsyncWrite + Unpin> FrameWriter<W> {
    /// The lock covers the entire frame, including its flush.  Closing the
    /// writer interrupts blocked writes and lock waiters; any partial frame
    /// belongs to a permanently closed stream.
    pub async fn write_frame(&self, bytes: &[u8]) -> io::Result<()> {
        let length = u32::try_from(bytes.len())
            .map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "Frame length exceeds u32"))?;
        let mut closed = self.failure();
        let result = tokio::select! {
            biased;
            _ = closed.wait_for(|failed| *failed) => {
                Err(io::Error::new(io::ErrorKind::BrokenPipe, "RPC output closed"))
            }
            result = async {
                let mut writer = self.writer.lock().await;
                writer.write_all(&length.to_be_bytes()).await?;
                writer.write_all(bytes).await?;
                writer.flush().await
            } => result,
        };
        if result.is_err() {
            self.close();
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::pin::Pin;
    use std::task::{Context, Poll};
    use tokio::io::AsyncReadExt;

    #[tokio::test]
    async fn concurrent_frames_do_not_interleave() {
        let (write, mut read) = tokio::io::duplex(128);
        let writer = FrameWriter::new(write);
        let (a, b) = tokio::join!(writer.write_frame(b"first"), writer.write_frame(b"second"));
        a.unwrap();
        b.unwrap();
        drop(writer);
        let mut bytes = Vec::new();
        read.read_to_end(&mut bytes).await.unwrap();
        assert!(
            bytes == b"\0\0\0\x05first\0\0\0\x06second"
                || bytes == b"\0\0\0\x06second\0\0\0\x05first"
        );
    }

    #[tokio::test]
    async fn close_interrupts_blocked_and_queued_frames() {
        let (write, mut read) = tokio::io::duplex(4);
        let writer = FrameWriter::new(write);
        let first = writer.write_frame(b"blocked payload");
        tokio::pin!(first);
        let mut header = [0; 4];
        tokio::select! {
            result = &mut first => panic!("frame completed before its payload drained: {result:?}"),
            _ = read.read_exact(&mut header) => {}
        }
        assert_eq!(u32::from_be_bytes(header), 15);
        let queued = writer.write_frame(b"queued");
        tokio::pin!(queued);
        assert!(futures::poll!(&mut queued).is_pending());
        // Leave the peer open but undrained.  Closure must not depend on it.
        writer.close();
        let (first, queued) = tokio::time::timeout(std::time::Duration::from_secs(1), async {
            tokio::join!(first, queued)
        })
        .await
        .expect("closing the writer must unblock every frame");
        assert_eq!(first.unwrap_err().kind(), io::ErrorKind::BrokenPipe);
        assert_eq!(queued.unwrap_err().kind(), io::ErrorKind::BrokenPipe);
    }

    struct FailAfterHeader {
        calls: usize,
    }

    impl AsyncWrite for FailAfterHeader {
        fn poll_write(
            mut self: Pin<&mut Self>,
            _: &mut Context<'_>,
            bytes: &[u8],
        ) -> Poll<io::Result<usize>> {
            self.calls += 1;
            Poll::Ready(if self.calls == 1 {
                Ok(bytes.len())
            } else {
                Err(io::ErrorKind::BrokenPipe.into())
            })
        }
        fn poll_flush(self: Pin<&mut Self>, _: &mut Context<'_>) -> Poll<io::Result<()>> {
            Poll::Ready(Ok(()))
        }
        fn poll_shutdown(self: Pin<&mut Self>, _: &mut Context<'_>) -> Poll<io::Result<()>> {
            Poll::Ready(Ok(()))
        }
    }

    #[tokio::test]
    async fn partial_frame_failure_closes_the_shared_writer() {
        let writer = FrameWriter::new(FailAfterHeader { calls: 0 });
        let mut failure = writer.failure();
        assert!(writer.write_frame(b"payload").await.is_err());
        failure.changed().await.unwrap();
        assert!(*failure.borrow());
        assert!(writer.write_frame(b"another frame").await.is_err());
        assert_eq!(writer.writer.lock().await.calls, 2);
    }
}
