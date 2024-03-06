
use std::fmt::Display;
use std::io::Write;
use std::path::Path;
use std::fs::{File, create_dir};

use proc_macro2::TokenStream;

/// A struct which represents a (sub)-filestructure.
#[derive(Debug, Eq, PartialEq)]
pub struct FileStructure {
	path: String,
	kind: FileType,
}

/// The types of file this filestructure can represent.
///
/// Because [`TokenStream`](proc_macro2::TokenStream)s are kinda funky and they just get converted to strings
/// for writing to disk anyway, they are not supported. Just convert them to string instead.
#[derive(Debug, Eq, PartialEq)]
pub enum FileType {
	/// Write a specific string to a file.
	String(String),
	/// Write binary data to a file.
	Blob(Box<[u8]>),
	/// This is a directory which holds multiple other files.
	Dir(Vec<FileStructure>)
}

impl Default for FileStructure {
    fn default() -> Self {
        Self::new(String::new(), FileType::Dir(vec![]))
    }
}

impl FileStructure {
	/// Basic constructor.
	#[must_use] 
	pub const fn new(path: String, kind: FileType) -> Self {
		Self { path, kind }
	}

	/// Constructs a dir.
	#[must_use]
	pub const fn new_dir(name: String) -> Self {
		Self { path: name, kind: FileType::Dir(Vec::new()) }
	}

	/// Given a unix path, splits it into it's separate components.
	///
	/// Will also check whether the path contains a starting `/`, returning `true` if so and `false` otherwise.
	///
	/// # Example
	/// ```ignore
	/// use wagon_codegen::FileStructure;
	/// let s1 = "/some/path";
	/// let s2 = "some/path";
	/// 
	/// assert_eq!(FileStructure::split_path(s1), (vec!["some".to_string(), "path".to_string()], true));
	/// assert_eq!(FileStructure::split_path(s2), (vec!["some".to_string(), "path".to_string()], false));
	/// ```
	fn split_path(path: &str) -> (Vec<String>, bool) {
		let mut iter = path.split('/');
		let mut ret = Vec::new();
		let is_root = iter.next().map_or(true, |s| 
			if s.is_empty() {
				true
			} else {
				ret.push(s.to_string());
				false
			});
		for sub in iter {
			if !sub.is_empty() {
				ret.push(sub.to_string());
			}
		}
		(ret, is_root)
	}

	fn _from_path(mut components: Vec<String>, is_root: bool) -> Self {
		components.pop().map_or_else(|| Self::new(String::new(), FileType::Dir(vec![])), |leaf_path| {
			let mut curr = Self::new(leaf_path, FileType::Dir(vec![]));
			while let Some(sub_dir) = components.pop() {
				let new = Self::new(sub_dir, FileType::Dir(vec![curr]));
				curr = new;
			}
			if is_root {
				Self::new(String::new(), FileType::Dir(vec![curr]))
			} else {
				curr
			}
		})
	}

	/// Constructs a `FileStructure` completely from a unix path.
	///
	/// If the path contains a starting `/`, will return a structure with a root node. If not, the root will be the first name of the path.
	///
	/// # Example
	/// ```
	/// use wagon_codegen::{FileStructure, FileType};
	/// let s = "/some/path";
	/// let fs = FileStructure::from_path(s);
	/// let expected = FileStructure::new("".to_string(), 
	///     FileType::Dir(vec![
	///         FileStructure::new("some".to_string(),
	///             FileType::Dir(vec![
	///                 FileStructure::new("path".to_string(),
	///                     FileType::Dir(vec![])
	///                 )
	///             ])
	///         )
	///     ])
	/// );
	/// assert_eq!(fs, expected);
	#[must_use]
	pub fn from_path(path: &str) -> Self {
		let (components, is_root) = Self::split_path(path);
		Self::_from_path(components, is_root)
	}

	/// Given a full `FileStructure` and a starting path, attempts to write it all to disk.
	///
	/// Directories are handled recursively. Everything else is converted to bytes and
	/// written to a specific file.
	///
	/// # Errors
	/// Errors whenever we fail to either write to a file or create a dir. See the
	/// documentation for [`File::create`], [`Write::write_all`] and [`create_dir`] for more
	/// info.
	pub fn write_to_disk(&self, root: &Path) -> std::io::Result<()> {
		let path = root.join(&self.path);
		match &self.kind {
		    FileType::String(s) => File::create(path)?.write_all(s.as_bytes())?,
		    FileType::Blob(b) => File::create(path)?.write_all(b)?,
		    FileType::Dir(fs) => {
		    	if !path.exists() {
		    		create_dir(path.clone())?;
		    	}
		    	for f in fs {
		    		f.write_to_disk(&path)?;
		    	}
		    },
		}
		Ok(())
	}

	/// Tries finding a file in the structure by its full path.
	///
	/// Path should be written unix style. 
	#[must_use] 
	pub fn get(&self, path: &str) -> Option<&Self> {
		let (components, is_root) = Self::split_path(path);
		let stop = components.len() - 1;
		let mut iter = components.iter().enumerate();
		if let Some((_, component)) = iter.next() { // Handle the first bit
			if &self.path != component {
				return None
			}
		} else if self.path.is_empty() && is_root {
			return Some(self)
		} else {
			return None
		};
		let mut curr = self;
		for (i, component) in iter {
			if let FileType::Dir(files) = &curr.kind {
        		if let Some(next) = files.iter().find(|file| &file.path == component) {
                    curr = next;
                } else {
                	return None;
                }
        	} else if i < stop { // We've reached the end of the tree but not the end of the path
     			return None;
     		}
		}
		Some(curr)
	}

	/// Tries finding a file in the structure by its full path, mutably
	///
	/// Path should be written unix style. 
	#[must_use] 
	pub fn get_mut(&mut self, path: &str) -> Option<&mut Self> {
		let (components, is_root) = Self::split_path(path);
		let stop = components.len() - 1;
		let mut iter = components.iter().enumerate();
		if let Some((_, component)) = iter.next() { // Handle the first bit
			if &self.path != component {
				return None
			}
		} else if self.path.is_empty() && is_root {
			return Some(self)
		} else {
			return None
		};
		let mut curr = self;
		for (i, component) in iter {
			if let FileType::Dir(ref mut files) = curr.kind {
        		if let Some(next) = files.iter_mut().find(|file| &file.path == component) {
                    curr = next;
                } else {
                	return None;
                }
        	} else if i < stop { // We've reached the end of the tree but not the end of the path
     			return None;
     		} else {
     			break; // This isn't needed for logic, but it is needed for the borrow checker.
     		}
		}
		Some(curr)
	}

	fn _insert_dir(&mut self, components: Vec<String>) -> Option<&mut Self> {
		let mut iter = components.into_iter();
		let mut curr = if let Some(component) = iter.next() {
			if self.path.is_empty() {
				if let FileType::Dir(ref mut files) = self.kind {
	        		if let Some(index) = files.iter().position(|file| file.path == component) { // Use the index to avoid borrow checker
	                    &mut files[index]
	                } else {
	                	let subdir = Self::new_dir(component);
						files.push(subdir);
						files.last_mut()?
	                }
	        	} else { // We've reached the end of the tree but not the end of the path and it's not a dir.
	     			return None;
	     		}
			} else if self.path != component {
				if let FileType::Dir(ref mut files) = self.kind { // Duplicate check because borrow checker complains.
					let subdir = Self::new_dir(component);
					files.push(subdir);
					files.last_mut()?
				} else {
					return None;
				}
			} else {
				self
			}
		} else {
			return Some(self)
		};
		for component in iter {
			if let FileType::Dir(ref mut files) = curr.kind {
        		if let Some(index) = files.iter().position(|file| file.path == component) { // Use the index to avoid borrow checker
                    curr = &mut files[index];
                } else {
                	let subdir = Self::new_dir(component); // Need to keep making dirs until we find the end.
					files.push(subdir);
					curr = files.last_mut()?;
                }
        	} else { // We've reached the end of the tree but not the end of the path and it's not a dir.
     			return None;
     		}
		}
		Some(curr)
	}

	fn insert_data(&mut self, path: &str, data: FileType) -> Option<&mut Self> {
		let (mut components, _) = Self::split_path(path);
		let filename = components.pop()?;
		let dir = self._insert_dir(components)?;
		dir.insert(Self::new(filename, data))
	}

	/// Inserts a directory into the `FileStructure`. Returns a `None` if we fail, returns a mutable reference to the bottom directory if we succeed.
	///
	/// Functions like `mkdir -p`, meaning that it will automatically create directories as needed until the full path has been added.
	pub fn insert_dir(&mut self, path: &str) -> Option<&mut Self> {
		let (components, _) = Self::split_path(path);
		self._insert_dir(components)
	}

	/// Insert binary blob data at some path relative to the root `FileStructure`.
	///
	/// Will automatically create directories if needed.
	pub fn insert_blob(&mut self, path: &str, blob: Box<[u8]>) -> Option<&mut Self> {
		self.insert_data(path, FileType::Blob(blob))
	}

	/// Insert a [`String`] at some path relative to the root `FileStructure`.
	///
	/// Will automatically create directories if needed.
	pub fn insert_string(&mut self, path: &str, data: String) -> Option<&mut Self> {
		self.insert_data(path, FileType::String(data))
	}

	/// Insert a [`TokenStream`] at some path relative to the root `FileStructure`.
	/// 
	/// The [`TokenStream`] will be converted to a [`String`]. If the `pretty` flag is set, the `TokenStream` will be prettified first.
	///
	/// Will automatically create directories if needed.
	///
	/// # Errors
	/// Returns a [`syn::parse::Error`] if the `pretty` flag is set and we fail to parse the [`TokenStream`].
	pub fn insert_tokenstream(&mut self, path: &str, data: TokenStream, pretty: bool) -> syn::parse::Result<Option<&mut Self>> {
		let data = if pretty {
			let ast: syn::File = syn::parse2(data)?;
			FileType::String(prettyplease::unparse(&ast))
		} else {
			FileType::String(data.to_string())
		};
		self.insert_data(path, data).map_or(Ok(None), |dir| Ok(Some(dir)))
	}

	/// Insert a `FileStructure` as a child to this `FileStructure`.
	///
	/// Returns a `None` if this `FileStructure` is not a directory. Returns a mutable reference to the child otherwise.
	pub fn insert(&mut self, child: Self) -> Option<&mut Self> {
		if let FileType::Dir(ref mut files) = self.kind {
			files.push(child);
			Some(files.last_mut()?)
		} else {
			None
		}
	}

	/// Get the path for this node of the structure.
	#[must_use] 
	pub fn get_path(&self) -> &str {
		&self.path
	}

	/// Get the "len" (file count) of the structure.
	///
	/// TODO: Precalculate this instead of exploring the whole tree.
	#[must_use]
	pub fn len(&self) -> usize {
		match &self.kind {
		    FileType::Dir(files) => {
		    	let mut sum = 0;
		    	for file in files {
		    		sum += file.len();
		    	}
		    	sum
		    },
		    _ => 1
		}
	}

	/// A filestructure is deemed empty if there are 0 and 0 subdirs in it.
	#[must_use]
	pub fn is_empty(&self) -> bool {
		self.len() == 0
	}

	/// Convert the `FileStructure` into a usable iterator.
	///
	/// This will iterate over all the nodes in a DFS style.
	#[must_use]
	pub fn iter(&self) -> FileIterator {
		FileIterator { stack: vec![self] }
	}

}

pub struct FileIterator<'a> {
	stack: Vec<&'a FileStructure>
}

pub struct OwnedFileIterator {
	stack: Vec<FileStructure>
}

impl<'a> Iterator for FileIterator<'a> {
    type Item = &'a FileStructure;

    fn next(&mut self) -> Option<Self::Item> {
        let cont = self.stack.pop()?;
        if let FileType::Dir(subdirs) = &cont.kind {
        	self.stack.extend(subdirs.iter());
        }
        Some(cont)
    }
}

impl Iterator for OwnedFileIterator {
    type Item = FileStructure;

    fn next(&mut self) -> Option<Self::Item> {
        let mut cont = self.stack.pop()?;
        if let FileType::Dir(ref mut subdirs) = cont.kind {
        	self.stack.extend(std::mem::take(subdirs));
        }
        Some(cont)
    }
}

impl IntoIterator for FileStructure {
    type Item = Self;

    type IntoIter = OwnedFileIterator;

    fn into_iter(self) -> Self::IntoIter {
        OwnedFileIterator { stack: vec![self] }
    }
}

impl<'a> IntoIterator for &'a FileStructure {
	type Item = Self;

	type IntoIter = FileIterator<'a>;

	fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl Display for FileStructure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            FileType::Dir(files) => {
            	writeln!(f, "{}: [", self.path)?;
            	for file in files {
            		writeln!(f, "{file},")?;
            	}
            	writeln!(f, "]")
            },
            _ => write!(f, "{}", self.path)
        }
    }
}

#[cfg(test)]
mod test {
	use super::{FileStructure, FileType};

	#[test]
	fn test_get() {
		let structure = FileStructure {
		    path: "some".to_string(),
		    kind: FileType::Dir(vec![
		        FileStructure {
		            path: "dir".to_string(),
		            kind: FileType::Dir(vec![
		            	FileStructure {
		            		path: "file.txt".to_string(),
		            		kind: FileType::Blob([].into()),
		            	},
		            	FileStructure {
		            		path: "other.txt".to_string(),
		            		kind: FileType::Blob([].into()),
		            	}
		            ]),
		        },
		        FileStructure {
		        	path: "other".to_string(),
		        	kind: crate::FileType::Dir(vec![
		        		FileStructure {
		        			path: "file.txt".to_string(),
		        			kind: FileType::Blob([].into()),
		        		}
		        	]),
		        }
		    ]),
		};
		let target = FileStructure {
    		path: "file.txt".to_string(),
    		kind: FileType::Blob([].into()),
    	};
    	assert_eq!(structure.get("some/dir/file.txt"), Some(&target));
    	assert_eq!(structure.get("some/dir/none.txt"), None);
    	assert_eq!(structure.get("nothing"), None);
	}

	#[test]
	fn test_insert() {
		let mut structure = FileStructure::from_path("some");
		structure.insert_dir("some/dir/");
		structure.insert_dir("some/other");
		structure.insert_blob("some/dir/file.txt", [].into()).unwrap();
		structure.insert_blob("some/dir/other.txt", [].into()).unwrap();
		structure.insert_blob("some/other/file.txt", [].into()).unwrap();
		let expected = FileStructure {
		    path: "some".to_string(),
		    kind: FileType::Dir(vec![
		        FileStructure {
		            path: "dir".to_string(),
		            kind: FileType::Dir(vec![
		            	FileStructure {
		            		path: "file.txt".to_string(),
		            		kind: FileType::Blob([].into()),
		            	},
		            	FileStructure {
		            		path: "other.txt".to_string(),
		            		kind: FileType::Blob([].into()),
		            	}
		            ]),
		        },
		        FileStructure {
		        	path: "other".to_string(),
		        	kind: crate::FileType::Dir(vec![
		        		FileStructure {
		        			path: "file.txt".to_string(),
		        			kind: FileType::Blob([].into()),
		        		}
		        	]),
		        }
		    ]),
		};
		assert_eq!(structure, expected);
	}

	#[test]
	fn test_insert_empty() {
		let mut structure = FileStructure::default();
		structure.insert_dir("some/dir/");
		structure.insert_dir("some/other");
		structure.insert_blob("some/dir/file.txt", [].into()).unwrap();
		structure.insert_blob("some/dir/other.txt", [].into()).unwrap();
		structure.insert_blob("some/other/file.txt", [].into()).unwrap();
		let expected = FileStructure {
			path: String::new(),
			kind: FileType::Dir(vec![
				FileStructure {
					path: "some".to_string(),
				    kind: FileType::Dir(vec![
				        FileStructure {
				            path: "dir".to_string(),
				            kind: FileType::Dir(vec![
				            	FileStructure {
				            		path: "file.txt".to_string(),
				            		kind: FileType::Blob([].into()),
				            	},
				            	FileStructure {
				            		path: "other.txt".to_string(),
				            		kind: FileType::Blob([].into()),
				            	}
				            ]),
				        },
				        FileStructure {
				        	path: "other".to_string(),
				        	kind: crate::FileType::Dir(vec![
				        		FileStructure {
				        			path: "file.txt".to_string(),
				        			kind: FileType::Blob([].into()),
				        		}
				        	]),
				        }
				    ]),
				}
			])
		};
		assert_eq!(structure, expected);
	}
}
