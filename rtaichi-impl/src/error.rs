pub type Result<T> = syn::Result<T>;

#[macro_export]
macro_rules! abort {
    ($span:expr, $($msg:tt)*) => {
        return Err(syn::Error::new(syn::spanned::Spanned::span($span), format!($($msg,)*)))
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
    ($es:expr => ($span:expr, $($msg:tt)*)) => {
        {
            let e = syn::Error::new(syn::spanned::Spanned::span($span), format!($($msg)*));
            $es.push(e);
            return Default::default();
        }
    }
}
#[macro_export]
macro_rules! abort_if {
    ($pred:expr, $es:expr => $res:block) => {
        { if $pred { abort!($es => $res); } }
    };
    ($pred:expr, $es:expr => ($span:expr, $($msg:tt)*)) => {
        { if $pred { abort!($es => ($span, $($msg)*)); } }
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
