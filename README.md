# foxglove

```dart
fn sort |a: [int], n: int|> {
    for i in 1..n {
        for j in 1..n {
            if a[i] < a[j] {
                var t = a[i];

                a[i] = a[j];
                a[j] = t;
            }
        }
    }
}

fn main ||> int {
    var a = [5, 3, 9, 1, 2, 4, 6, 10, 7, 8];
    var n = 10;

    sort(a, n);

    return 0;
}
```
