pub type Result<T> = syn::Result<T>;

#[macro_export]
macro_rules! abort {
    ($span:expr, $($msg:expr),*) => {
        {
            let m = format!($($msg),*);
            println!("{} ({:?})", &m, $span);
            return Err(syn::Error::new(syn::spanned::Spanned::span($span), m));
        }
    };
    ($es:expr => $res:block) => {
        match $res {
            Ok(x) => x,
            Err(e) => {
                $es.push(e);
                return Default::default();
            },
        }
    };
    ($es:expr => ($span:expr, $($msg:expr),*)) => {
        {
            let m = format!($($msg),*);
            println!("{} ({:?})", &m, $span);
            let e = syn::Error::new(syn::spanned::Spanned::span($span), m);
            $es.push(e);
            return Default::default();
        }
    };
    ($es:expr => $res:expr) => {
        match { $res } {
            Ok(x) => x,
            Err(e) => {
                $es.push(e);
                return Default::default();
            },
        }
    };
}
#[macro_export]
macro_rules! abort_if {
    ($pred:expr, $es:expr => $res:block) => {
        { if $pred { abort!($es => $res); } }
    };
    ($pred:expr, $es:expr => ($span:expr, $($msg:tt)*)) => {
        { if $pred { abort!($es => ($span, $($msg)*)); } }
    };
    ($pred:expr, ($span:expr, $($msg:tt)*)) => {
        { if $pred { abort!($span, $($msg)*); } }
    };
}

#[macro_export]
macro_rules! abort_scope {
    ($es:expr => $body:block) => {
        match ((|| -> Result<()> {$body; Ok(())})()) {
            Err(e) => $es.push(e),
            Ok(()) => {},
        }
    };
}

#[derive(Default)]
pub struct ErrorStore {
    errors: Vec<syn::Error>,
}
impl ErrorStore {
    pub fn new() -> Self{
        Default::default()
    }

    pub fn push(&mut self, e: syn::Error) {
        self.errors.push(e);
    }
    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    pub fn panic(&self) {
        if !self.errors.is_empty() {
            panic!("{:?}", self.errors);
        }
    }
}
impl Extend<syn::Error> for ErrorStore {
    fn extend<T: IntoIterator<Item = syn::Error>>(&mut self, iter: T) {
        self.errors.extend(iter);
    }
}
impl IntoIterator for ErrorStore {
    type Item = syn::Error;
    type IntoIter = std::vec::IntoIter<Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        self.errors.into_iter()
    }
}
