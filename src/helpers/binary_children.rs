
#[derive(Debug, PartialEq)]
pub struct BinaryChildren<T> {
	left: Option<T>,
	right: Option<T>
}

impl<T> BinaryChildren<T> {

	pub fn push(&mut self, child: T) {
		if self.left.is_some() {
			if self.right.is_some() {
				panic!("{:?}", "Tried to push too many children onto binary relation");
			} else {
				self.right = Some(child);
			}
		} else {
			self.left = Some(child);
		}
	}

	pub fn pop(&mut self) -> Option<T> {
		if self.right.is_some() {
			self.right.take()
		} else if self.left.is_some() {
			self.left.take()
		} else {
			None
		}
	}

	pub fn collect(&self) -> [Option<&T>; 2] {
		if self.right.is_some() {
			[self.left.as_ref(), self.right.as_ref()]
		} else if self.left.is_some() {
			[self.left.as_ref(), None]
		} else {
			[None, None]
		}
	}

	pub fn into_vec(self) -> Vec<T> {
		if self.right.is_some() {
			vec![self.left.unwrap(), self.right.unwrap()]
		} else if self.left.is_some() {
			vec![self.left.unwrap()]
		} else {
			Vec::with_capacity(0)
		}
	}
}

impl<T> Default for BinaryChildren<T> {
    fn default() -> Self {
        Self {left: None, right: None}
    }
}