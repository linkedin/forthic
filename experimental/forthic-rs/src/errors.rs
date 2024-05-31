#[derive(Debug)]
pub struct InterpreterError {
    pub message: String,
    pub filename: String,
    pub line: u32,
}

#[macro_export]
macro_rules! interpreter_error {
    ($msg:expr) => {
        InterpreterError {
            message: $msg.to_string(),
            filename: file!().to_string(),
            line: line!(),
        }
    };
}
