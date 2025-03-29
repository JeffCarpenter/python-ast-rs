/// The Python Object. Anything that implements this trait is a Python Object.
pub trait Object: Sized {
    /// Returns the unique identifier of the object, which is the memory address of the object.
    fn id(&self) -> usize {
        std::ptr::addr_of!(*self) as usize
    }

    /// Returns the type name of the object.
    fn r#type(&self) -> String {
        std::any::type_name::<Self>().to_string()
    }

    /// Returns the attributes of the object.
    fn dir(&self) -> Vec<&'static str> {
        vec![
            "__class__",
            "__class_getitem__",
            "__contains__",
            "__delattr__",
            "__dict__",
            "__dir__",
            "__doc__",
            "__eq__",
            "__format__",
            "__ge__",
            "__getattribute__",
            "__gt__",
            "__hash__",
            "__init__",
            "__init_subclass__",
            "__iter__",
            "__le__",
            "__lt__",
            "__module__",
            "__ne__",
            "__new__",
            "__reduce__",
            "__reduce_ex__",
            "__repr__",
            "__setattr__",
            "__sizeof__",
            "__str__",
            "__subclasshook__",
            "__weakref__",
            "clear",
            "copy",
            "fromkeys",
            "get",
            "items",
            "keys",
            "pop",
            "popitem",
            "setdefault",
            "update",
            "values",
        ]
    }
}
