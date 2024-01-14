use std::iter::Peekable;

/// A trait version of the functionality that [`Peekable`] provides.
///
/// Something like this has been in RFC for a while, but it's not getting implemented. 
/// For our own purposes, we implement it ourselves.
///
/// [`Peekable`] implements this trait by calling inner methods of the same name.
///
/// NOTE: The documentation of these methods are taken directly from the [`Peekable`] documentation.
pub trait Peek: Iterator {
    /// Returns a reference to the next() value without advancing the iterator.
    ///
    /// Like [`next`], if there is a value, it is wrapped in a `Some(T)`.
    /// But if the iteration is over, `None` is returned.
    ///
    /// [`next`]: Iterator::next
    ///
    /// Because `peek()` returns a reference, and many iterators iterate over
    /// references, there can be a possibly confusing situation where the
    /// return value is a double reference. You can see this effect in the
    /// examples below.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let xs = [1, 2, 3];
    ///
    /// let mut iter = xs.iter().peekable();
    ///
    /// // peek() lets us see into the future
    /// assert_eq!(iter.peek(), Some(&&1));
    /// assert_eq!(iter.next(), Some(&1));
    ///
    /// assert_eq!(iter.next(), Some(&2));
    ///
    /// // The iterator does not advance even if we `peek` multiple times
    /// assert_eq!(iter.peek(), Some(&&3));
    /// assert_eq!(iter.peek(), Some(&&3));
    ///
    /// assert_eq!(iter.next(), Some(&3));
    ///
    /// // After the iterator is finished, so is `peek()`
    /// assert_eq!(iter.peek(), None);
    /// assert_eq!(iter.next(), None);
    /// ```
    fn peek(&mut self) -> Option<&Self::Item>;
    /// Returns a mutable reference to the next() value without advancing the iterator.
    ///
    /// Like [`next`], if there is a value, it is wrapped in a `Some(T)`.
    /// But if the iteration is over, `None` is returned.
    ///
    /// Because `peek_mut()` returns a reference, and many iterators iterate over
    /// references, there can be a possibly confusing situation where the
    /// return value is a double reference. You can see this effect in the examples
    /// below.
    ///
    /// [`next`]: Iterator::next
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let mut iter = [1, 2, 3].iter().peekable();
    ///
    /// // Like with `peek()`, we can see into the future without advancing the iterator.
    /// assert_eq!(iter.peek_mut(), Some(&mut &1));
    /// assert_eq!(iter.peek_mut(), Some(&mut &1));
    /// assert_eq!(iter.next(), Some(&1));
    ///
    /// // Peek into the iterator and set the value behind the mutable reference.
    /// if let Some(p) = iter.peek_mut() {
    ///     assert_eq!(*p, &2);
    ///     *p = &5;
    /// }
    ///
    /// // The value we put in reappears as the iterator continues.
    /// assert_eq!(iter.collect::<Vec<_>>(), vec![&5, &3]);
    /// ```
    fn peek_mut(&mut self) -> Option<&mut Self::Item>;
    /// Consume and return the next value of this iterator if a condition is true.
    ///
    /// If `func` returns `true` for the next value of this iterator, consume and return it.
    /// Otherwise, return `None`.
    ///
    /// # Examples
    /// Consume a number if it's equal to 0.
    /// ```
    /// let mut iter = (0..5).peekable();
    /// // The first item of the iterator is 0; consume it.
    /// assert_eq!(iter.next_if(|&x| x == 0), Some(0));
    /// // The next item returned is now 1, so `consume` will return `false`.
    /// assert_eq!(iter.next_if(|&x| x == 0), None);
    /// // `next_if` saves the value of the next item if it was not equal to `expected`.
    /// assert_eq!(iter.next(), Some(1));
    /// ```
    ///
    /// Consume any number less than 10.
    /// ```
    /// let mut iter = (1..20).peekable();
    /// // Consume all numbers less than 10
    /// while iter.next_if(|&x| x < 10).is_some() {}
    /// // The next value returned will be 10
    /// assert_eq!(iter.next(), Some(10));
    /// ```
    fn next_if(&mut self, func: impl FnOnce(&Self::Item) -> bool) -> Option<Self::Item> {
        match self.peek() {
            Some(matched) if func(matched) => Some(self.next()?),
            _ => None
        }
    }
    /// Consume and return the next item if it is equal to `expected`.
    ///
    /// # Example
    /// Consume a number if it's equal to 0.
    /// ```
    /// let mut iter = (0..5).peekable();
    /// // The first item of the iterator is 0; consume it.
    /// assert_eq!(iter.next_if_eq(&0), Some(0));
    /// // The next item returned is now 1, so `consume` will return `false`.
    /// assert_eq!(iter.next_if_eq(&0), None);
    /// // `next_if_eq` saves the value of the next item if it was not equal to `expected`.
    /// assert_eq!(iter.next(), Some(1));
    /// ```
    fn next_if_eq<T>(&mut self, expected: &T) -> Option<Self::Item>
    where
        T: ?Sized,
        Self::Item: PartialEq<T>,
    {
        self.next_if(|next| next == expected)
    }
}

impl<I: Iterator> Peek for Peekable<I> {
    fn peek(&mut self) -> Option<&Self::Item> {
        self.peek()
    }

    fn peek_mut(&mut self) -> Option<&mut Self::Item> {
        self.peek_mut()
    }

    fn next_if(&mut self, func: impl FnOnce(&Self::Item) -> bool) -> Option<Self::Item> {
        self.next_if(func)
    }
}
