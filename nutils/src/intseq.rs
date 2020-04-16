#[derive(PartialEq, Debug)]
pub struct IndexOutOfBounds;

#[derive(Clone)]
pub struct IndexArray(std::sync::Arc<Vec<usize>>);

impl IndexArray {
  pub fn new(indices: Vec<usize>) -> Self {
    Self(std::sync::Arc::new(indices))
  }
  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }
  pub fn len(&self) -> usize {
    self.0.len()
  }
  pub fn get(&self, index: usize) -> Result<usize, IndexOutOfBounds> {
    match self.0.get(index) {
      Some(value) => Ok(*value),
      None => Err(IndexOutOfBounds),
    }
  }
  pub fn take(&self, indices: &Self) -> Result<Self, IndexOutOfBounds> {
    let mut values: Vec<usize> = Vec::with_capacity(indices.len());
    for index in &*indices.0 {
      values.push(self.get(*index)?);
    }
    Ok(Self::new(values))
  }
  pub fn unchain(&self, value: usize) -> (Self, Self) {
    (Self::new(self.0.iter().cloned().filter(|x| x < &value).collect()), Self::new(self.0.iter().cloned().filter(|x| x >= &value).map(|x| x - value).collect()))
  }
  pub fn max(&self) -> Option<usize> {
    Some(*self.0.iter().max()?)
  }
  pub fn is_smaller_than(&self, value: usize) -> bool {
    match self.max() {
      Some(max) => max < value,
      None => true,
    }
  }
}

#[test]
fn test_index_array_get() {
  let a = IndexArray::new(vec![0, 1, 2]);
  assert_eq!(a.get(0), Ok(0));
  assert_eq!(a.get(1), Ok(1));
  assert_eq!(a.get(2), Ok(2));
  assert_eq!(a.get(3), Err(IndexOutOfBounds));
}

#[test]
fn test_index_array_take() {
  let a = IndexArray::new(vec![0, 1, 2, 3]);
  let b = IndexArray::new(vec![1, 2]);
  let c = a.take(&b).unwrap();
  assert_eq!(c.get(0), Ok(1));
  assert_eq!(c.get(1), Ok(2));
  assert_eq!(c.get(2), Err(IndexOutOfBounds));
}

fn divmod(index: usize, length: usize) -> (usize, usize) {
  if length > 0 {
    (index / length, index % length)
  } else {
    (index, 0)
  }
}

enum Data {
  Empty,
  Uniform { value: i64, length: usize },
  Vector { values: Vec<i64> },
  Repeat { sequence: IntSequence, count: usize },
  Take { sequence: IntSequence, indices: IndexArray },
  Chain { sequence1: IntSequence, sequence2: IntSequence },
  Product { sequence1: IntSequence, sequence2: IntSequence },
}

#[derive(Clone)]
pub struct IntSequence(std::sync::Arc<Data>);

impl IntSequence {
  pub fn empty() -> Self {
    Self(std::sync::Arc::new(Data::Empty))
  }
  pub fn uniform(value: i64, length: usize) -> Self {
    Self(std::sync::Arc::new(Data::Uniform { value, length }))
  }
  pub fn from_vector(values: Vec<i64>) -> Self {
    Self(std::sync::Arc::new(Data::Vector { values }))
  }
  fn new_repeat(sequence: Self, count: usize) -> Self {
    Self(std::sync::Arc::new(Data::Repeat { sequence, count }))
  }
  fn new_take(sequence: Self, indices: IndexArray) -> Self {
    Self(std::sync::Arc::new(Data::Take { sequence, indices }))
  }
  fn new_chain(sequence1: Self, sequence2: Self) -> Self {
    Self(std::sync::Arc::new(Data::Chain { sequence1, sequence2 }))
  }
  fn new_product(sequence1: Self, sequence2: Self) -> Self {
    Self(std::sync::Arc::new(Data::Product { sequence1, sequence2 }))
  }
  fn unchain(sequence: &Self, into: &mut Vec<IntSequence>) {
    match &*sequence.0 {
      Data::Chain { sequence1, sequence2 } => {
        Self::unchain(sequence1, into);
        Self::unchain(sequence2, into);
      }
      _ => {
        into.push(sequence.clone());
      }
    };
  }
  pub fn is_empty(&self) -> bool {
    self.len() == 0
  }
  pub fn len(&self) -> usize {
    match &*self.0 {
      Data::Empty => 0,
      Data::Uniform { length, .. } => *length,
      Data::Vector { values } => values.len(),
      Data::Repeat { sequence, count } => sequence.len() * count,
      Data::Take { indices, .. } => indices.len(),
      Data::Chain { sequence1, sequence2 } => sequence1.len() + sequence2.len(),
      Data::Product { sequence1, sequence2 } => sequence1.len() * sequence2.len(),
    }
  }
  pub fn get(&self, index: usize) -> Result<i64, IndexOutOfBounds> {
    match &*self.0 {
      Data::Empty => Err(IndexOutOfBounds),
      Data::Uniform { value, length } => {
        if index < *length {
          Ok(*value)
        } else {
          Err(IndexOutOfBounds)
        }
      }
      Data::Vector { values } => match values.get(index) {
        Some(value) => Ok(*value),
        None => Err(IndexOutOfBounds),
      },
      Data::Repeat { sequence, count } => {
        let (index1, index2) = divmod(index, sequence.len());
        if index1 >= *count {
          Err(IndexOutOfBounds)
        } else {
          sequence.get(index2)
        }
      }
      Data::Take { sequence, indices } => sequence.get(indices.get(index)?),
      Data::Chain { sequence1, sequence2 } => {
        if index < sequence1.len() {
          sequence1.get(index)
        } else {
          sequence2.get(index - sequence1.len())
        }
      }
      Data::Product { sequence1, sequence2 } => {
        let (index1, index2) = divmod(index, sequence2.len());
        Ok(sequence1.get(index1)? * sequence2.get(index2)?)
      }
    }
  }
  pub fn take(&self, indices: &IndexArray) -> Result<Self, IndexOutOfBounds> {
    if indices.is_empty() {
      return Ok(Self::empty());
    }
    match &*self.0 {
      Data::Uniform { value, .. } => {
        if indices.is_smaller_than(self.len()) {
          Ok(Self::uniform(*value, indices.len()))
        } else {
          Err(IndexOutOfBounds)
        }
      }
      Data::Take { sequence, indices: sequence_indices } => Ok(sequence.take(&sequence_indices.take(indices)?)?),
      Data::Chain { sequence1, sequence2 } => {
        let (indices1, indices2) = indices.unchain(sequence1.len());
        Ok(sequence1.take(&indices1)?.chain(&sequence2.take(&indices2)?))
      }
      _ => {
        if indices.is_smaller_than(self.len()) {
          Ok(Self::new_take(self.clone(), indices.clone()))
        } else {
          Err(IndexOutOfBounds)
        }
      }
    }
  }
  pub fn chain(&self, other: &Self) -> Self {
    if other.is_empty() {
      self.clone()
    } else if self.is_empty() {
      other.clone()
    } else {
      match &*self.0 {
        Data::Chain { sequence1, sequence2 } => Self::new_chain(sequence1.clone(), sequence2.chain(other)),
        _ => Self::new_chain(self.clone(), other.clone()),
      }
    }
  }
  pub fn product(&self, other: &Self) -> Self {
    match &*self.0 {
      Data::Product { sequence1, sequence2 } => sequence1.product(&sequence2.product(other)),
      _ => Self::new_product(self.clone(), other.clone()),
    }
  }
  pub fn repeat(&self, count: usize) -> Self {
    match &*self.0 {
      Data::Empty => Self::empty(),
      Data::Uniform { value, length } => Self::uniform(*value, *length * count),
      Data::Repeat { sequence, count: sequencecount } => Self::new_repeat(sequence.clone(), *sequencecount * count),
      _ => Self::new_repeat(self.clone(), count),
    }
  }
}

#[cfg(test)]
mod tests {

  macro_rules! common {
    (@setup $seqv:ident=$seq:expr, $checkv:ident=$check:tt) => {
      let $seqv = $seq;
      let $checkv: Vec<i64> = vec!$check;
    };
    ($name:ident, seq=$seq:expr, check=$check:tt) => {
      mod $name {
        use super::super::{IntSequence, IndexOutOfBounds};

        #[test]
        fn len() {
          common!(@setup seq=$seq, check=$check);
          assert_eq!(seq.len(), check.len());
        }

        #[test]
        fn get() {
          common!(@setup seq=$seq, check=$check);
          for (i, val) in check.iter().enumerate() {
            assert_eq!(seq.get(i), Ok(*val));
          }
        }

        #[test]
        fn get_out_of_bounds() {
          common!(@setup seq=$seq, check=$check);
          assert_eq!(seq.get(check.len()), Err(IndexOutOfBounds));
        }

        #[test]
        fn repeat() {
          common!(@setup seq=$seq, check=$check);
          let rep = seq.repeat(3);
          assert_eq!(rep.len(), check.len()*3);
          for (i, val) in check.iter().enumerate() {
            assert_eq!(rep.get(i), Ok(*val));
            assert_eq!(rep.get(check.len()+i), Ok(*val));
            assert_eq!(rep.get(2*check.len()+i), Ok(*val));
          }
          assert_eq!(rep.get(3*check.len()), Err(IndexOutOfBounds));
        }

        #[test]
        fn chain_empty() {
          common!(@setup seq=$seq, check=$check);
          let chn = seq.chain(&IntSequence::empty());
          assert_eq!(chn.len(), check.len());
          for (i, val) in check.iter().enumerate() {
            assert_eq!(chn.get(i), Ok(*val));
          }
          assert_eq!(chn.get(check.len()), Err(IndexOutOfBounds));
        }

        #[test]
        fn chain_self() {
          common!(@setup seq=$seq, check=$check);
          let chn = seq.chain(&seq);
          assert_eq!(chn.len(), 2*check.len());
          for (i, val) in check.iter().enumerate() {
            assert_eq!(chn.get(i), Ok(*val));
            assert_eq!(chn.get(check.len()+i), Ok(*val));
          }
          assert_eq!(chn.get(2*check.len()), Err(IndexOutOfBounds));
        }

        #[test]
        fn chain_other() {
          common!(@setup seq=$seq, check=$check);
          let oth = IntSequence::from_vector(vec![3, 1, 2]);
          let chn = seq.chain(&oth);
          assert_eq!(chn.len(), check.len()+3);
          for (i, val) in check.iter().enumerate() {
            assert_eq!(chn.get(i), Ok(*val));
          }
          assert_eq!(chn.get(check.len()+0), Ok(3));
          assert_eq!(chn.get(check.len()+1), Ok(1));
          assert_eq!(chn.get(check.len()+2), Ok(2));
          assert_eq!(chn.get(check.len()+3), Err(IndexOutOfBounds));
        }

        #[test]
        fn product_empty() {
          common!(@setup seq=$seq, _check=$check);
          let prd = seq.product(&IntSequence::empty());
          assert_eq!(prd.len(), 0);
          assert_eq!(prd.get(0), Err(IndexOutOfBounds));
        }

        #[test]
        fn product_self() {
          common!(@setup seq=$seq, check=$check);
          let prd = seq.product(&seq);
          assert_eq!(prd.len(), check.len()*check.len());
          for (i, v) in check.iter().enumerate() {
            for (j, w) in check.iter().enumerate() {
              assert_eq!(prd.get(check.len()*i+j), Ok(v*w));
            }
          }
          assert_eq!(prd.get(check.len()*check.len()), Err(IndexOutOfBounds));
        }

        #[test]
        fn product_other() {
          common!(@setup seq=$seq, check=$check);
          let oth = IntSequence::from_vector(vec![3, 1, 2]);
          let prd = seq.product(&oth);
          assert_eq!(prd.len(), 3*check.len());
          for (i, v) in check.iter().enumerate() {
            assert_eq!(prd.get(3*i), Ok(v*3));
            assert_eq!(prd.get(3*i+1), Ok(v*1));
            assert_eq!(prd.get(3*i+2), Ok(v*2));
          }
          assert_eq!(prd.get(3*check.len()), Err(IndexOutOfBounds));
        }

      }
    }
  }

  common!(empty, seq = IntSequence::empty(), check = []);

  common!(uniform, seq = IntSequence::uniform(1, 3), check = [1, 1, 1]);

  common!(vector, seq = IntSequence::from_vector(vec![1, 2, 3]), check = [1, 2, 3]);
}
