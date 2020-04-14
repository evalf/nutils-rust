mod lexer {

  #[derive(PartialEq, Debug)]
  pub struct NoMatch;

  pub trait Token: Sized {
    fn parse(text: &str, start: usize) -> Result<(Self, usize), NoMatch>;
    fn len(&self) -> usize;
  }

  #[derive(PartialEq, Debug)]
  pub struct EOF;

  impl Token for EOF {
    fn parse(text: &str, start: usize) -> Result<(Self, usize), NoMatch> {
      match start == text.len() {
        true => Ok((Self, start)),
        false => Err(NoMatch)
      }
    }
    fn len(&self) -> usize { 0 }
  }

  #[derive(PartialEq, Debug)]
  pub struct Repeat0<T>(pub Vec<T>) where T: Token;

  impl<T> Token for Repeat0<T> where T: Token {
    fn parse(text: &str, start: usize) -> Result<(Self, usize), NoMatch> {
      let mut v = Vec::new();
      let mut i = start;
      loop {
        match T::parse(text, i) {
          Ok((t, j)) => { v.push(t); i = j; },
          Err(_e) => { break; }
        }
      }
      Ok((Self(v), i))
    }
    fn len(&self) -> usize {
      self.0.iter().map(|item| item.len()).sum()
    }
  }

  #[derive(PartialEq, Debug)]
  pub struct Repeat1<T>(pub Vec<T>) where T: Token;

  impl<T> Token for Repeat1<T> where T: Token {
    fn parse(text: &str, start: usize) -> Result<(Self, usize), NoMatch> {
      let mut v = Vec::new();
      let (t0, mut i) = T::parse(text, start)?;
      v.push(t0);
      loop {
        match T::parse(text, i) {
          Ok((t, j)) => { v.push(t); i = j; },
          Err(_e) => { break; }
        }
      }
      Ok((Self(v), i))
    }
    fn len(&self) -> usize {
      self.0.iter().map(|item| item.len()).sum()
    }
  }

  impl<T> Token for Option<T> where T: Token {
    fn parse(text: &str, start: usize) -> Result<(Self, usize), NoMatch> {
      match T::parse(text, start) {
        Ok((v, stop)) => Ok((Some(v), stop)),
        Err(_e) => Ok((None, start))
      }
    }
    fn len(&self) -> usize {
      match self {
        Some(v) => v.len(),
        None => 0
      }
    }
  }

  impl<T> Token for Box<T> where T: Token {
    fn parse(text: &str, start: usize) -> Result<(Self, usize), NoMatch> {
      match T::parse(text, start) {
        Ok((v, stop)) => Ok((Box::new(v), stop)),
        Err(e) => Err(e)
      }
    }
    fn len(&self) -> usize {
      T::len(self)
    }
  }

  macro_rules! literal {
    ($name:ident = $value:literal) => {
      #[derive(PartialEq, Debug)]
      pub struct $name;
      impl Token for $name {
        fn parse(text: &str, start: usize) -> Result<(Self, usize), NoMatch> {
          match text[start..].starts_with($value) {
            true => Ok((Self, start+$value.len())),
            false => Err(NoMatch)
          }
        }
        fn len(&self) -> usize { $value.len() }
      }
    }
  }

//macro_rules! char_cat {
//  ($name:ident, $cat:ident) => {
//    pub struct $name(pub char);
//    impl Token for $name {
//      fn test(text: &str, start: usize) -> Result<(Self, usize), NoMatch> {
//        match text[start..].chars().next() {
//          Some(ch) => match ch.$cat() {
//            true => Ok(Match { start: start, stop: start+TODO, value: Self { ch: ch } }),
//            false => Err(NoMatch)
//          },
//          None => Err(NoMatch)
//        }
//      }
//    }
//  }
//}

  macro_rules! ascii_char_test {
    ($ch:ident is $name:ident if $test:expr) => {
      #[derive(PartialEq, Debug)]
      pub struct $name;
      impl Token for $name {
        fn parse(text: &str, start: usize) -> Result<(Self, usize), NoMatch> {
          match text[start..].bytes().next() {
            Some($ch) => match $test {
              true => Ok((Self, start+1)),
              false => Err(NoMatch)
            },
            None => Err(NoMatch)
          }
        }
        fn len(&self) -> usize { 1 }
      }
    }
  }

  macro_rules! impl_token_for_tuple {
    ($($term:ident),+) => {
      impl<$($term: Token),+> Token for ($($term,)+) {
        fn parse(text: &str, start: usize) -> Result<(Self, usize), NoMatch> {
          let mut i = start;
          Ok((($({ let (v, j) = <$term>::parse(text, i)?; i = j; v }),+,), i))
        }
        fn len(&self) -> usize {
          #[allow(non_snake_case)]
          let ($($term,)+) = self;
          0 $(+ $term.len())+
        }
      }
    };
  }
  impl_token_for_tuple!{A}
  impl_token_for_tuple!{A,B}
  impl_token_for_tuple!{A,B,C}
  impl_token_for_tuple!{A,B,C,D}
  impl_token_for_tuple!{A,B,C,D,E}

  macro_rules! rule {
    (@len $s:ident: $A:ty) => { $s.0.len() };
    (@len $s:ident: $A:ty, $B:ty) => { $s.0.len() + $s.1.len() };
    (@len $s:ident: $A:ty, $B:ty, $C:ty) => { $s.0.len() + $s.1.len() + $s.2.len() };
    (@len $s:ident: $A:ty, $B:ty, $C:ty, $D:ty) => { $s.0.len() + $s.1.len() + $s.2.len() + $s.3.len() };
    (@len $s:ident: $A:ty, $B:ty, $C:ty, $D:ty, $E:ty) => { $s.0.len() + $s.1.len() + $s.2.len() + $s.3.len() + $s.4.len() };
    (@enump $alt:path: $a:ident, $b:ident, $c:ident, $d:ident, $e:ident: $A:ty) => { $alt($a) };
    (@enump $alt:path: $a:ident, $b:ident, $c:ident, $d:ident, $e:ident: $A:ty, $B:ty) => { $alt($a, $b) };
    (@enump $alt:path: $a:ident, $b:ident, $c:ident, $d:ident, $e:ident: $A:ty, $B:ty, $C:ty) => { $alt($a, $b, $c) };
    (@enump $alt:path: $a:ident, $b:ident, $c:ident, $d:ident, $e:ident: $A:ty, $B:ty, $C:ty, $D:ty) => { $alt($a, $b, $c, $d) };
    (@enump $alt:path: $a:ident, $b:ident, $c:ident, $d:ident, $e:ident: $A:ty, $B:ty, $C:ty, $D:ty, $E:ty) => { $alt($a, $b, $c, $d, $e) };
    (@enumv $a:ident, $b:ident, $c:ident, $d:ident, $e:ident: $A:ty) => { $a.len() };
    (@enumv $a:ident, $b:ident, $c:ident, $d:ident, $e:ident: $A:ty, $B:ty) => { $a.len() + $b.len() };
    (@enumv $a:ident, $b:ident, $c:ident, $d:ident, $e:ident: $A:ty, $B:ty, $C:ty) => { $a.len() + $b.len() + $c.len() };
    (@enumv $a:ident, $b:ident, $c:ident, $d:ident, $e:ident: $A:ty, $B:ty, $C:ty, $D:ty) => { $a.len() + $b.len() + $c.len() + $d.len() };
    (@enumv $a:ident, $b:ident, $c:ident, $d:ident, $e:ident: $A:ty, $B:ty, $C:ty, $D:ty, $E:ty) => { $a.len() + $b.len() + $c.len() + $d.len() + $e.len() };
    ($name:ident ::= $($term:ty),+) => {
      #[derive(PartialEq, Debug)]
      pub struct $name($(pub $term),+);
      impl Token for $name {
        fn parse(text: &str, start: usize) -> Result<(Self, usize), NoMatch> {
          let mut i = start;
          Ok((Self($({ let (v, j) = <$term>::parse(text, i)?; i = j; v }),+), i))
        }
        fn len(&self) -> usize { rule!(@len self: $($term),+) }
      }
    };
    ($name:ident ::= $($alt:ident: $($term:ty),+)|+) => {
      #[derive(PartialEq, Debug)]
      pub enum $name { $($alt($($term),+)),+ }
      impl Token for $name {
        fn parse(text: &str, start: usize) -> Result<(Self, usize), NoMatch> {
          $({
            loop {
              let mut i = start;
              return Ok((Self::$alt($(match <$term>::parse(text, i) { Ok((v, j)) => { i = j; v }, Err(_e) => break }),+), i));
            }
          })+
          return Err(NoMatch);
        }
        fn len(&self) -> usize { match self { $(rule!(@enump Self::$alt: a, b, c, d, e: $($term),+) => rule!(@enumv a, b, c, d, e: $($term),+) ),+ } }
      }
    };
  }

  ascii_char_test!{ch is Alpha if (ch & 0xdf) >= 65 && (ch & 0xdf) <= 90}
  ascii_char_test!{ch is AlphaNum if (ch & 0xdf) >= 65 && (ch & 0xdf) <= 90 || ch >= 48 && ch <= 57}
  ascii_char_test!{ch is Digit if ch >= 48 && ch <= 57}
  ascii_char_test!{ch is NonZeroDigit if ch >= 49 && ch <= 57}
  ascii_char_test!{ch is Zero if ch == 48}
  ascii_char_test!{ch is Space if ch == 32}
  ascii_char_test!{ch is Plus if ch == 43}
  ascii_char_test!{ch is Minus if ch == 45}
  ascii_char_test!{ch is Underscore if ch == 95}
  ascii_char_test!{ch is Comma if ch == 44}
  ascii_char_test!{ch is Semicolon if ch == 59}
  ascii_char_test!{ch is ParenOpen if ch == 40}
  ascii_char_test!{ch is ParenClose if ch == 41}
  ascii_char_test!{ch is Carret if ch == 94}
  ascii_char_test!{ch is Slash if ch == 47}

  rule!{Expression        ::= SubExpression, EOF}
  rule!{SubExpression     ::= Option<Minus>, Repeat0<Space>, ProductOrFraction, Repeat0<(Repeat1<Space>, Operator, Repeat1<Space>, ProductOrFraction)>}
  rule!{ProductOrFraction ::= Product: Product
                            | Fraction: Product, Space, Slash, Space, Product}
  rule!{Product           ::= Item, Repeat0<(Space, Item)>}
  rule!{Operator          ::= Plus: Plus
                            | Minus: Minus}
  rule!{Item              ::= Base: ItemBase
                            | Power: ItemBase, Carret, RealNumber}
  rule!{ItemBase          ::= NonNegConstant: NonNegConstant
                            | Variable: Variable
                            | Scoped: ParenOpen, Box<SubExpression>, ParenClose, Option<(Underscore, Derivatives)>}
  rule!{Constant          ::= RealNumber, Option<(Underscore, IndexVariables)>}
  rule!{NonNegConstant    ::= NonNegRealNumber, Option<(Underscore, IndexVariables)>}
  // TODO: float, scientific notation
  rule!{RealNumber        ::= Zero: Zero
                            | NonZero: Option<Minus>, NonZeroDigit, Repeat0<Digit>}
  rule!{NonNegRealNumber  ::= Zero: Zero
                            | Positive: NonZeroDigit, Digit}
  rule!{Variable          ::= Scalar: Name, Option<(Underscore, Derivatives)>
                            | Array: Name, Underscore, Indices, Option<Derivatives>}
  rule!{Name              ::= Alpha, Repeat0<AlphaNum>}
  rule!{Derivatives       ::= DerivType, Indices}
  rule!{DerivType         ::= Regular: Comma
                            | Surface: Semicolon}
  rule!{Indices           ::= Repeat1<Index>}
  rule!{IndexVariables    ::= Repeat1<IndexVariable>}
  rule!{Index             ::= Constant: IndexConstant
                            | Variable: IndexVariable}
  rule!{IndexConstant     ::= Digit}
  rule!{IndexVariable     ::= Alpha}

  #[cfg(test)]
  mod tests {
    use super::*;

    #[test]
    fn it_works() {
      assert_eq!(Operator::parse("+", 0), Ok((Operator::Plus(Plus), 1)));
      assert_eq!(RealNumber::parse("0", 0), Ok((RealNumber::Zero(Zero), 1)));
      println!("{:?}", Expression::parse("(-a + b) c", 0));
    }
  }
}

mod ast {

  use crate::lexer;
  use crate::lexer::Token;

  // enum AST {
  //   ConstScalar(f64),
  //   Group(Box<AST>),
  //   Arg(String, Vec<usize>),
  //   Substitute(Box<AST>, ...),
  //
  //   Add(Box<AST>, Box<AST>),
  // }

  // enum Length {
  //   Known(usize),
  //   Unknown(usize) // the argument is the position of the index in the expression string
  // }

  // struct Array {
  //   ast: AST,
  //   indices: Vec<char>,
  //   shape: Vec<Length>,
  //   summed: std::collections::HashSet(char),
  //   linked_lengths: ...
  // }

  trait Eval {
    type T;
    fn eval(&self, text: &str, start: usize) -> Result<(Self::T, usize), String>;
  }

  macro_rules! impl_eval {
    (@= $arg:ident, $text:ident, $i:ident) => {
      {
        $i += $arg.len();
        $arg
      }
    };
    (@! $arg:ident, $text:ident, $i:ident) => {
      {
        let (v, j) = $arg.eval($text, $i)?;
        $i = j;
        v
      }
    };
    ($Token:path: $($op:tt$arg:tt),+ -> $Ret:ty $body:block) => {
      impl Eval for $Token {
        type T = $Ret;
        fn eval(&self, text: &str, start: usize) -> Result<(Self::T, usize), String> {
          let mut i = start;
          let ($($arg,)*) = {
            let $Token($($arg),+) = self;
            ($(impl_eval!(@$op $arg, text, i),)*)
          };
          match $body {
            Ok(v) => Ok((v, i)),
            Err(e) => Err(e)
          }
        }
      }
    };
    ($Token:path: $Ret:ty { $($alt:ident: $($op:tt$arg:tt),+ => $body:block),+ }) => {
      impl Eval for $Token {
        type T = $Ret;
        fn eval(&self, text: &str, start: usize) -> Result<(Self::T, usize), String> {
          match self {$(
            Self::$alt($($arg),+) => {
              let mut i = start;
              let ($($arg,)*) = ($(impl_eval!(@$op $arg, text, i),)+);
              match $body {
                Ok(v) => Ok((v, i)),
                Err(e) => Err(e)
              }
            },
          )+}
        }
      }
    };
  }

  impl<T> Eval for lexer::Repeat0<T> where T: Eval + lexer::Token {
    type T = Vec<T::T>;
    fn eval(&self, text: &str, start: usize) -> Result<(Self::T, usize), String> {
      let mut i = start;
      let mut v = Vec::new();
      for token in self.0.iter() {
        let (item, j) = token.eval(text, i)?;
        i = j;
        v.push(item);
      }
      Ok((v, i))
    }
  }

  impl<T> Eval for lexer::Repeat1<T> where T: Eval + lexer::Token {
    type T = Vec<T::T>;
    fn eval(&self, text: &str, start: usize) -> Result<(Self::T, usize), String> {
      let mut i = start;
      let mut v = Vec::new();
      for token in self.0.iter() {
        let (item, j) = token.eval(text, i)?;
        i = j;
        v.push(item);
      }
      Ok((v, i))
    }
  }

  impl<T: Eval> Eval for Option<T> {
    type T = Option<T::T>;
    fn eval(&self, text: &str, start: usize) -> Result<(Self::T, usize), String> {
      match self {
        Some(v) => { let (v, stop) = v.eval(text, start)?; Ok((Some(v), stop)) },
        None => Ok((None, start))
      }
    }
  }

  impl<T: Eval> Eval for Box<T> {
    type T = T::T;
    fn eval(&self, text: &str, start: usize) -> Result<(Self::T, usize), String> {
      T::eval(&*self, text, start)
    }
  }

  macro_rules! impl_eval_for_tuple {
    ($($term:ident),+) => {
      impl<$($term: Eval),+> Eval for ($($term,)+) {
        type T = ($($term::T,)+);
        fn eval(&self, text: &str, start: usize) -> Result<(Self::T, usize), String> {
          let mut i = start;
          #[allow(non_snake_case)]
          let ($($term,)+) = self;
          Ok((($({ let (v, j) = $term.eval(text, i)?; i = j; v },)+), i))
        }
      }
    };
  }
  impl_eval_for_tuple!{A}
  impl_eval_for_tuple!{A,B}
  impl_eval_for_tuple!{A,B,C}
  impl_eval_for_tuple!{A,B,C,D}
  impl_eval_for_tuple!{A,B,C,D,E}

  impl Eval for lexer::Name {
    type T = String;
    fn eval(&self, text: &str, start: usize) -> Result<(Self::T, usize), String> {
      let stop = start + self.0.len() + self.1.len();
      Ok((text[start..stop].to_string(), stop))
    }
  }

  impl Eval for lexer::RealNumber {
    type T = f64;
    fn eval(&self, text: &str, start: usize) -> Result<(Self::T, usize), String> {
      let stop = start + self.len();
      Ok((text[start..stop].parse().unwrap(), stop))
    }
  }

  impl Eval for lexer::NonNegRealNumber {
    type T = f64;
    fn eval(&self, text: &str, start: usize) -> Result<(Self::T, usize), String> {
      let stop = start + self.len();
      Ok((text[start..stop].parse().unwrap(), stop))
    }
  }

  #[derive(PartialEq, Debug)]
  enum Derivatives { Regular(Vec<Index>), Surface(Vec<Index>) }

  impl_eval!{lexer::Derivatives: =typ, !indices -> Derivatives {
    Ok(match typ {
      lexer::DerivType::Regular(_) => Derivatives::Regular(indices),
      lexer::DerivType::Surface(_) => Derivatives::Surface(indices)
    })
  }}

  impl_eval!{lexer::Indices: !indices -> Vec<Index> { Ok(indices) }}
  impl_eval!{lexer::IndexVariables: !indices -> Vec<char> { Ok(indices) }}

  #[derive(PartialEq, Debug)]
  enum Index { Constant(u8), Variable(char) }

  impl_eval!{lexer::Index: Index {
    Constant: !v => { Ok(Index::Constant(v)) },
    Variable: !v => { Ok(Index::Variable(v)) }
  }}

  impl Eval for lexer::IndexConstant {
    type T = u8;
    fn eval(&self, text: &str, start: usize) -> Result<(Self::T, usize), String> {
      Ok((text[start..].bytes().next().unwrap() - 48, start+self.0.len()))
    }
  }

  impl Eval for lexer::IndexVariable {
    type T = char;
    fn eval(&self, text: &str, start: usize) -> Result<(Self::T, usize), String> {
      Ok((text[start..].chars().next().unwrap(), start+self.0.len()))
    }
  }

  #[cfg(test)]
  mod tests {

    use crate::lexer;
    use super::*;

    macro_rules! parse_eval {
      ($token:ty, $text:expr) => {
        <$token>::parse($text, 0).unwrap().0.eval($text, 0)
      }
    }

    #[test]
    fn test() {
      assert_eq!(parse_eval!(lexer::Name, "test"), Ok(("test".to_string(), 4)));
      assert_eq!(parse_eval!(lexer::IndexVariable, "i"), Ok(('i', 1)));
      assert_eq!(parse_eval!(lexer::IndexConstant, "0"), Ok((0, 1)));
      assert_eq!(parse_eval!(lexer::Index, "0"), Ok((Index::Constant(0), 1)));
      assert_eq!(parse_eval!(lexer::Index, "i"), Ok((Index::Variable('i'), 1)));
      assert_eq!(parse_eval!(lexer::Derivatives, ",1i"), Ok((Derivatives::Regular(vec![Index::Constant(1), Index::Variable('i')]), 3)));
      assert_eq!(parse_eval!(lexer::RealNumber, "1"), Ok((1f64, 1)));
    }

  }

}
