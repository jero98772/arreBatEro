use pyo3::prelude::*;
use pyo3::exceptions;

/// Returns the nth Fibonacci number.
///
/// # Examples
///
/// ```
/// let result = fibonacci(5);
/// assert_eq!(result, 5);
/// ```
///
/// # Panics
///
/// Panics if `n` is negative.
///
/// # Errors
///
/// Returns an error if `n` is too large to calculate Fibonacci.
#[pyfunction]
fn fibonacci(n: u64) -> PyResult<u64> {
    if n == 0 {
        Ok(0)
    } else if n == 1 {
        Ok(1)
    } else {
        let mut a: u64 = 0;
        let mut b: u64 = 1;
        for _ in 2..=n {
            let c = match a.checked_add(b) {
                Some(value) => value,
                None => return Err(PyErr::new::<exceptions::PyValueError, _>("overflow")),
            };
            a = b;
            b = c;
        }
        Ok(b)
    }
}

/// Checks if a number is prime.
///
/// # Examples
///
/// ```
/// assert_eq!(is_prime(7), true);
/// ```
///
/// # Panics
///
/// Panics if `n` is negative.
#[pyfunction]
fn is_prime(n: u64) -> bool {
    if n <= 1 {
        return false;
    }
    for i in 2..=(n as f64).sqrt() as u64 {
        if n % i == 0 {
            return false;
        }
    }
    true
}

/// Prints the nth number of the Fibonacci sequence.
///
/// # Examples
///
/// ```
/// print_fibonacci(5);
/// // Output: 5
/// ```
///
/// # Panics
///
/// Panics if `n` is negative.
#[pyfunction]
fn print_fibonacci(n: u64) {
    match fibonacci(n) {
        Ok(result) => println!("{}", result),
        Err(e) => eprintln!("Error: {}", e),
    }
}

#[pymodule]
fn mymodule(py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(fibonacci, m)?)?;
    m.add_function(wrap_pyfunction!(is_prime, m)?)?;
    m.add_function(wrap_pyfunction!(print_fibonacci, m)?)?;
    Ok(())
}
