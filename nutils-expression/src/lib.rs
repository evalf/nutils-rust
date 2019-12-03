#[derive(PartialEq, Debug)]
struct Match<T> {
  value: T,
  start: usize,
  stop: usize,
}

#[derive(PartialEq, Debug)]
struct NoMatch;

trait Term: Sized {
  fn test(text: &str, start: usize) -> Result<Match<Self>, NoMatch>;
}

macro_rules! literal {
  ($name:ident ::= $value:literal) => {
    #[derive(PartialEq, Debug)]
    struct $name {
      value: &'static str,
    }
    impl Term for $name {
      fn test(text: &str, start: usize) -> Result<Match<Self>, NoMatch> {
        match text[start..].starts_with($value) {
          true => Ok(Match { value: Self { value: $value }, start: start, stop: start+$value.len() }),
          false => Err(NoMatch),
        }
      }
    }
  }
}

#[derive(PartialEq, Debug)]
struct Lit;

macro_rules! _typeof {
  ($value:literal) => { Lit };
  ($value:ty) => { $value };
}

macro_rules! _test {
  ($value:literal, $text:expr, $i:expr) => { match $text[$i..].starts_with($value) { true => Ok(Match{ value: Lit, start: $i, stop: $i+$value.len() }), false => Err(NoMatch) } };
  ($value:ty, $text:expr, $i:expr) => { <$value>::test($text, $i) };
}

macro_rules! rule {
  ($name:ident ::= $($m:tt),+) => {
    #[derive(PartialEq, Debug)]
    struct $name( $( _typeof!($m) ),+ );
    impl Term for $name {
      fn test(text: &str, start: usize) -> Result<Match<Self>, NoMatch> {
        let mut i = start;
        //Ok(Match { value: Self( $( { let v = <$m>::test(text, i)?; i = v.stop; v.value } ),+ ), start: start, stop: i })
        Ok(Match { value: Self( $( { let v = _test!($m, text, i)?; i = v.stop; v.value } ),+ ), start: start, stop: i })
      }
    }
  }
}

literal!(Space ::= " ");
//rule!(Name ::= Space Space);
rule!(Name ::= Space, " ", "a");
// rule!(Name ::= a:Space, b:" "*, c:"a");
// rule!(Name ::= A(a:Space, b:" ", c:"a")
//              | B(x:Space, b:" ")

#[cfg(test)]
mod tests {
  use super::*;
  #[test]
  fn it_works() {
    assert_eq!(Space::test(" ", 0), Ok(Match { value: Space { value: " " }, start: 0, stop: 1 }));
    assert_eq!(Name::test("  a", 0), Ok(Match { start: 0, stop: 3, value: Name(Space { value: " " }, Lit, Lit) }));
    //assert_eq!(eof(" ", 1), R::Match(Match { value: EOF {}, start: 1, stop: 1 }));
  }
}
