
use std::cmp::Ordering;

use itertools::Itertools;

extern crate alloc;

mod extrema_set;

/// A slightly modified version of the [`Itertools`] trait which returns [`Result`] instead.
///
/// Automatically implemented for anything that implements [`Itertools`]. This trait is intended
/// for methods which require fallible operations (comparisons, additions etc.). Not all
/// [`Itertools`] methods have been ported over. More may be added as needed.
pub trait FallibleItertools: Itertools {
    /// Return all minimum elements of an iterator, as determined by
    /// the specified function. Or an error if the comparison fails.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::cmp::Ordering;
    /// use wagon_utils::FallibleItertools;
    ///
    /// let a: [(i32, i32); 0] = [];
    /// assert_eq!(a.iter().fallible_min_set_by(|_, _| Ok::<std::cmp::Ordering, ()>(Ordering::Equal)), Ok(Vec::<&(i32, i32)>::new()));
    ///
    /// let a = [(1, 2)];
    /// assert_eq!(a.iter().fallible_min_set_by(|&&(k1,_), &&(k2, _)| Ok::<std::cmp::Ordering, ()>(k1.cmp(&k2))), Ok(vec![&(1, 2)]));
    ///
    /// let a = [(1, 2), (2, 2), (3, 9), (4, 8), (5, 9)];
    /// assert_eq!(a.iter().fallible_min_set_by(|&&(_,k1), &&(_,k2)| Err("error")), Err("error"));
    ///
    /// let a = [(1, 2), (1, 3), (1, 4), (1, 5)];
    /// assert_eq!(a.iter().fallible_min_set_by(|&&(k1,_), &&(k2, _)| Ok::<std::cmp::Ordering, ()>(k1.cmp(&k2))), Ok(vec![&(1, 2), &(1, 3), &(1, 4), &(1, 5)]));
    /// ```
    ///
    /// The elements can be floats but no particular result is guaranteed
    /// if an element is NaN.
    ///
    /// # Errors
    /// Returns an error if the comparison function returns an error at any point.
    fn fallible_min_set_by<F, E>(self, mut compare: F) -> Result<Vec<Self::Item>, E>
    where
        Self: Sized,
        F: FnMut(&Self::Item, &Self::Item) -> Result<Ordering, E>,
    {
        extrema_set::min_set_impl(self, |_| (), |x, y, (), ()| compare(x, y))
    }

    /// Return all maximum elements of an iterator, as determined by
    /// the specified function.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::cmp::Ordering;
    /// use wagon_utils::FallibleItertools;
    ///
    /// let a: [(i32, i32); 0] = [];
    /// assert_eq!(a.iter().fallible_max_set_by(|_, _| Ok::<std::cmp::Ordering, ()>(Ordering::Equal)), Ok(Vec::<&(i32, i32)>::new()));
    ///
    /// let a = [(1, 2)];
    /// assert_eq!(a.iter().fallible_max_set_by(|&&(k1,_), &&(k2, _)| Ok::<std::cmp::Ordering, ()>(k1.cmp(&k2))), Ok(vec![&(1, 2)]));
    ///
    /// let a = [(1, 2), (2, 2), (3, 9), (4, 8), (5, 9)];
    /// assert_eq!(a.iter().fallible_max_set_by(|&&(_,k1), &&(_,k2)| Err("error")), Err("error"));
    ///
    /// let a = [(1, 2), (1, 3), (1, 4), (1, 5)];
    /// assert_eq!(a.iter().fallible_max_set_by(|&&(k1,_), &&(k2, _)| Ok::<std::cmp::Ordering, ()>(k1.cmp(&k2))), Ok(vec![&(1, 2), &(1, 3), &(1, 4), &(1, 5)]));
    /// ```
    ///
    /// The elements can be floats but no particular result is guaranteed
    /// if an element is NaN.
    ///
    /// # Errors
    /// Returns an error if the comparison function returns an error at any point.
    fn fallible_max_set_by<F, E>(self, mut compare: F) -> Result<Vec<Self::Item>, E>
    where
        Self: Sized,
        F: FnMut(&Self::Item, &Self::Item) -> Result<Ordering, E>,
    {
        extrema_set::max_set_impl(self, |_| (), |x, y, (), ()| compare(x, y))
    }
}

impl<T> FallibleItertools for T where T: Itertools {}