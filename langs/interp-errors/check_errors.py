# Build run/main_err.rs, and check that an Err from Rust's main is
# written to the standard error with {:?}, and that the exit code is 1.
# run/main_err.rs を作り、Rust の main が返した Err が、標準エラーに
# {:?} で書かれ、終了コードが 1 になることを確かめる
import os, subprocess, tempfile

here = os.path.dirname(os.path.abspath(__file__))
rustc = os.environ.get("BOOK_RUSTC", "rustc")

with tempfile.TemporaryDirectory() as tmp:
    exe = os.path.join(tmp, "main_err")
    subprocess.run([rustc, "--edition", "2024", "-o", exe,
                    os.path.join(here, "run", "main_err.rs")], check=True)
    done = subprocess.run([exe], capture_output=True, text=True)
    assert done.returncode == 1, done
    assert done.stdout == "the shop opens\n", done.stdout
    assert done.stderr == 'Error: "orders.txt: no such file"\n', done.stderr
print("ok main_err.rs")
