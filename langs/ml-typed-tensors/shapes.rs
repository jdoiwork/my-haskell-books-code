// The typed matrix of the chapter "Keeping dimensions in types", in Rust,
// with const generics: the numbers of rows and columns are in the type.
// 「型で次元を守る」の章の、型のある行列を Rust で。
// const generics で、行と列の数を型に書く

// ANCHOR: matrix
// Rust
#[derive(Debug, Clone, Copy, PartialEq)]
struct Matrix<const R: usize, const C: usize>([[f64; C]; R]);

impl<const R: usize, const C: usize> Matrix<R, C> {
    // (R × C) times (C × K) is (R × K).
    // (R × C) と (C × K) の積は (R × K)
    fn matmul<const K: usize>(&self, other: &Matrix<C, K>) -> Matrix<R, K> {
        let mut out = [[0.0; K]; R];
        for i in 0..R {
            for j in 0..K {
                for k in 0..C {
                    out[i][j] += self.0[i][k] * other.0[k][j];
                }
            }
        }
        Matrix(out)
    }

    fn transpose(&self) -> Matrix<C, R> {
        let mut out = [[0.0; R]; C];
        for i in 0..R {
            for j in 0..C {
                out[j][i] = self.0[i][j];
            }
        }
        Matrix(out)
    }
}
// ANCHOR_END: matrix

fn main() {
    // ANCHOR: use
    let m: Matrix<2, 3> = Matrix([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]);
    let square: Matrix<2, 2> = m.matmul(&m.transpose());
    // let wrong = m.matmul(&m);  // error: expected `Matrix<3, _>`
    // ANCHOR_END: use
    assert_eq!(square, Matrix([[14.0, 32.0], [32.0, 77.0]]));
    println!("ok");
}
