pub const Outer = struct {
    value: u32,

    pub fn outerMethod(self: Outer) u32 {
        return self.value;
    }

    pub const Middle = struct {
        data: u64,

        pub fn middleMethod(self: Middle) u64 {
            return self.data;
        }

        pub const Inner = struct {
            flag: bool,

            pub fn innerMethod(self: Inner) bool {
                return self.flag;
            }
        };
    };
};
