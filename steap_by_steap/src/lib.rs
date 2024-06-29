// src/lib.rs
use pyo3::prelude::*;

mod graph;
mod tree;
mod unionfind;
use unionfind::UnionFind;


#[pymodule]
fn steap_by_steap(m: &PyModule) -> PyResult<()> {
    m.add_class::<UnionFind>()?;

    Ok(())
}
