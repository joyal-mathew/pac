use std::result;

macro_rules! parse_rule {
    (name = $name: ident, parent = $parent: ident, $($operator: pat => $operation: expr),*) => {
        fn $name(&mut self) -> Result<Expression> {
            let mut node = self.$parent()?;

            loop {
                match self.current {
                    $( Token::Operator($operator) => {
                        self.advance()?;
                        node = Expression::BinaryOperation($operation, Box::new(node), Box::new(self.$parent()?));
                    } )*
                    _ => break,
                }
            }

            Ok(node)
        }
    };
}

macro_rules! err {
    ($($arg: tt)*) => (Err(format!("Error from {}:{} -- ", file!(), line!()) + &format!($($arg)*)));
}

macro_rules! expect {
    ($self: ident, $token: pat) => {
        if let $token = $self.current {
            $self.advance()?;
        }
        else {
            return err!("expected {:?}, got {:?}", stringify!($token), $self.current);
        }
    };
}

macro_rules! emit {
    ($self: ident, $($arg: tt)*) => ($self.emit(&format!($($arg)*)));
}

pub type Result<T> = result::Result<T, String>;

pub trait Serial {
    fn serialize(self) -> [u8; 8];
    fn deserialize(bytes: [u8; 8]) -> Self;
}

impl Serial for u64 {
    fn serialize(self) -> [u8; 8] { self.to_le_bytes() }
    fn deserialize(bytes: [u8; 8]) -> u64 { Self::from_le_bytes(bytes) }
}

impl Serial for i64 {
    fn serialize(self) -> [u8; 8] { self.to_le_bytes() }
    fn deserialize(bytes: [u8; 8]) -> i64 { Self::from_le_bytes(bytes) }
}

impl Serial for f64 {
    fn serialize(self) -> [u8; 8] { self.to_le_bytes() }
    fn deserialize(bytes: [u8; 8]) -> f64 { Self::from_le_bytes(bytes) }
}
