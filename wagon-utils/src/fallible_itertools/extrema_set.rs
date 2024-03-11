
use super::alloc::{vec, vec::Vec};
use std::cmp::Ordering;

/// Implementation guts for `min_set`, `min_set_by`, and `min_set_by_key`.
/// 
/// This was taken directly from [`itertools`] and then modified to return [`Result`] instead.
pub fn min_set_impl<I, K, F, E, Compare>(
    mut it: I,
    mut key_for: F,
    mut compare: Compare,
) -> Result<Vec<I::Item>, E>
where
    I: Iterator,
    F: FnMut(&I::Item) -> K,
    Compare: FnMut(&I::Item, &I::Item, &K, &K) -> Result<Ordering, E>,
{
    match it.next() {
        None => Ok(Vec::new()),
        Some(element) => {
            let mut current_key = key_for(&element);
            let mut result = vec![element];
            for element in it {
                let key = key_for(&element);
                match compare(&element, &result[0], &key, &current_key)? {
                    Ordering::Less => {
                        result.clear();
                        result.push(element);
                        current_key = key;
                    }
                    Ordering::Equal => {
                        result.push(element);
                    }
                    Ordering::Greater => {}
                }
            };
            Ok(result)
        }
    }
}

/// Implementation guts for `max_set`, `max_set_by`, and `max_set_by_key`.
pub fn max_set_impl<I, K, F, E, Compare>(it: I, key_for: F, mut compare: Compare) -> Result<Vec<I::Item>, E>
where
    I: Iterator,
    F: FnMut(&I::Item) -> K,
    Compare: FnMut(&I::Item, &I::Item, &K, &K) -> Result<Ordering, E>,
{
    min_set_impl(it, key_for, |it1, it2, key1, key2| {
        compare(it2, it1, key2, key1)
    })
}
