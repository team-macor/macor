use educe::Educe;
use postcard::experimental::max_size::MaxSize;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use std::io::{Read, Write};
use std::marker::PhantomData;
use std::net::{Shutdown, SocketAddr, TcpListener, TcpStream, ToSocketAddrs};
use tracing::{info, instrument, warn};

pub trait Channel<I, O>: Sized {
    type Recipient;
    type Error: std::error::Error;
    type Addr;

    fn connect(recipient: Self::Recipient) -> Result<Self, Self::Error>;
    fn listen(address: Self::Addr) -> Result<Self, Self::Error>;

    fn send(&mut self, msg: O) -> Result<(), Self::Error>;
    fn receive(&mut self) -> Result<I, Self::Error>;

    fn close(self) -> Result<(), Self::Error>;
}

#[derive(MaxSize, Serialize, Deserialize)]
struct Header {
    body_size: usize,
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("IO")]
    Io(#[from] std::io::Error),

    #[error("Serialization")]
    Serialization(#[from] postcard::Error),
}

#[derive(Educe)]
#[educe(Debug)]
pub struct TcpChannel<I, O> {
    stream: TcpStream,
    #[educe(Debug(ignore))]
    marker: PhantomData<(I, O)>,
}

impl<I, O> Channel<I, O> for TcpChannel<I, O>
where
    I: DeserializeOwned + std::fmt::Debug,
    O: Serialize + std::fmt::Debug,
{
    type Recipient = SocketAddr;
    type Error = Error;
    type Addr = SocketAddr;

    #[instrument]
    fn connect(recipient: Self::Recipient) -> Result<Self, Self::Error> {
        let stream = TcpStream::connect(recipient)?;

        info!("connected to {recipient:?}");

        Ok(TcpChannel {
            stream,
            marker: PhantomData,
        })
    }

    #[instrument]
    fn listen(address: Self::Addr) -> Result<Self, Self::Error> {
        let a = format!("{address:?}");
        info!("listening on {a}");
        let listener = TcpListener::bind(address)?;
        let (stream, _) = listener.accept()?;
        info!("got connection on {a}");

        Ok(TcpChannel {
            stream,
            marker: PhantomData,
        })
    }

    fn send(&mut self, msg: O) -> Result<(), Self::Error> {
        info!("Sending {msg:?}");

        let body = postcard::to_allocvec(&msg)?;
        let mut header_buf = [0; Header::POSTCARD_MAX_SIZE];

        let header = Header {
            body_size: body.len(),
        };
        postcard::to_slice(&header, &mut header_buf).expect(
            "Header buf should have been sized according to maximum possible serialization size",
        );

        self.stream.write_all(&header_buf)?;
        self.stream.write_all(&body)?;

        Ok(())
    }

    fn receive(&mut self) -> Result<I, Self::Error> {
        let mut header_buf = [0; Header::POSTCARD_MAX_SIZE];

        if let Err(e) = self.stream.read_exact(&mut header_buf) {
            tracing::error!("Failed to get message header: {e:?}");
            panic!("Failed to get the message header");
        }

        let header: Header =
            postcard::from_bytes(&header_buf).expect("Failed to deserialize message header");

        let mut body_buf = vec![0; header.body_size];
        self.stream.read_exact(&mut body_buf)?;

        let msg = postcard::from_bytes(&body_buf)?;
        info!("Received {msg:?}");

        Ok(msg)
    }

    fn close(self) -> Result<(), Self::Error> {
        self.stream.shutdown(Shutdown::Both)?;

        Ok(())
    }
}

#[derive(Educe)]
#[educe(Debug)]
pub struct TcpJsonChannel<I, O> {
    stream: TcpStream,
    #[educe(Debug(ignore))]
    marker: PhantomData<(I, O)>,
}

impl<I, O> Channel<I, O> for TcpJsonChannel<I, O>
where
    I: DeserializeOwned + std::fmt::Debug,
    O: Serialize + std::fmt::Debug,
{
    type Recipient = SocketAddr;
    type Error = Error;
    type Addr = SocketAddr;

    #[instrument]
    fn connect(recipient: Self::Recipient) -> Result<Self, Self::Error> {
        let stream = TcpStream::connect(recipient)?;

        info!("connected to {recipient:?}");

        Ok(TcpJsonChannel {
            stream,
            marker: PhantomData,
        })
    }

    #[instrument]
    fn listen(address: Self::Addr) -> Result<Self, Self::Error> {
        let a = format!("{address:?}");
        info!("listening on {a}");
        let listener = TcpListener::bind(address)?;
        let (stream, _) = listener.accept()?;
        info!("got connection on {a}");

        Ok(TcpJsonChannel {
            stream,
            marker: PhantomData,
        })
    }

    fn send(&mut self, msg: O) -> Result<(), Self::Error> {
        info!("Sending {msg:?}");

        let body = serde_json::to_string(&msg).unwrap();
        let mut header_buf = [0; Header::POSTCARD_MAX_SIZE];

        let header = Header {
            body_size: body.len(),
        };
        postcard::to_slice(&header, &mut header_buf).expect(
            "Header buf should have been sized according to maximum possible serialization size",
        );

        self.stream.write_all(&header_buf)?;
        self.stream.write_all(&body.as_bytes())?;

        Ok(())
    }

    fn receive(&mut self) -> Result<I, Self::Error> {
        let mut header_buf = [0; Header::POSTCARD_MAX_SIZE];

        if let Err(e) = self.stream.read_exact(&mut header_buf) {
            tracing::error!("Failed to get message header: {e:?}");
            panic!("Failed to get the message header");
        }

        let header: Header =
            postcard::from_bytes(&header_buf).expect("Failed to deserialize message header");

        let mut body_buf = vec![0; header.body_size];
        self.stream.read_exact(&mut body_buf)?;

        let msg = serde_json::from_slice(&body_buf).unwrap();
        info!("Received {msg:?}");

        Ok(msg)
    }

    fn close(self) -> Result<(), Self::Error> {
        self.stream.shutdown(Shutdown::Both)?;

        Ok(())
    }
}
