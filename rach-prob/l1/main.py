import numpy as np
import typing as typ
import numpy.typing as npt
from numpy.random import Generator, MT19937
import matplotlib.pyplot as plt
import argparse as arg
from typing import Union, Protocol

# Type aliases
FloatArray = npt.NDArray[np.float64]
BoolArray = npt.NDArray[np.bool_]
IntArray = npt.NDArray[np.int32]
VectorVType = Union[float, FloatArray]


# Vectorized function type: accepts both scalars and vectors (arrays)
class VectorizedFn(Protocol):
    def __call__(self, x: VectorVType) -> VectorVType: ...


TransformType = typ.Callable[[FloatArray], FloatArray]
FnCheckVArg = Union[typ.Tuple[float, float], typ.Tuple[FloatArray, FloatArray]]
FnCheckType = typ.Callable[[VectorizedFn, FnCheckVArg], Union[bool, BoolArray]]


class MonteCarloIntegral:
    """
    Monte Carlo Integration
    """

    N_MIN = 50
    N_MAX = 5000
    N_STEP = 50

    sup: float
    a: float
    b: float
    fn: VectorizedFn
    gen: Generator
    is_negative: bool
    checkFn: FnCheckType

    def __init__(self, fn: VectorizedFn, a: float, b: float, sup: float | None = None, checkFn: FnCheckType | None = None):
        self.fn = fn
        self.fn = fn
        self.a = a
        self.b = b

        # Calculate maximum/minimum value on [a, b] for the provided function
        if sup == None:
            def calc_deriv(x: VectorVType) -> VectorVType:
                D_EPSILON = 0.0001
                y = self.fn(x)
                dy = self.fn(x + D_EPSILON)
                return (dy - y) / D_EPSILON

            # Choose the most promising starting point
            # and then use gradient ascent or descent to find the max/min value
            xs = np.linspace(a, b, 20)
            ys = fn(xs)
            self.is_negative = bool(np.all(ys <= 0))
            if np.any(ys < 0) and np.any(ys > 0):
                raise ValueError(
                    'Function has alternating values (between positive and negative)')

            x = 0.0

            # Use gradient ascent to find maximum
            d = 0.0
            EPSILON = 0.01
            ALPHA = 0.15
            N_STEPS = 20
            i = 0

            if self.is_negative:
                x = float(xs[np.argmin(ys)])
            else:
                x = float(xs[np.argmax(ys)])

            while abs(d) > EPSILON and x < b and x > a and i < N_STEPS:
                i += 1
                d = calc_deriv(x)
                if d > 0 and not self.is_negative:
                    x += ALPHA * d
                    x = min(x, b)
                elif d < 0 and self.is_negative:
                    x -= ALPHA * d
                    x = max(a, x)
                else:
                    break

            # Evaluate function at the found maximum/minimum point
            sup = float(fn(x))

        if checkFn == None:
            # Check if the point(s) is under the curve
            def defaultCheck(f: VectorizedFn, v: FnCheckVArg) -> Union[bool, BoolArray]:
                xs, ys = v
                y = f(xs)

                if self.is_negative:
                    return y <= ys
                return y >= ys

            checkFn = defaultCheck

        self.sup = sup
        self.checkFn = checkFn
        self.gen = Generator(MT19937())

    def as_plt(self, k: int, data: typ.Dict[int, FloatArray]) -> typ.Tuple[FloatArray, FloatArray]:
        """
        Converts the result of 'go(...)' to a values,keys np arrays, that may be plotted on graph
        """
        values = np.array([arr for arr in data.values()]).flatten()
        keys = np.array([[key]*k for key in data.keys()]).flatten()
        return values, keys

    def n_range(self, n_min: int = N_MIN, n_max: int = N_MAX, n_step: int = N_STEP) -> IntArray:
        return np.arange(n_min, n_max+n_step, n_step)

    def go(self, k: int = 5, n_min: int = N_MIN, n_max: int = N_MAX, n_step: int = N_STEP) -> typ.Dict[int, FloatArray]:
        print(f'k={k} n_min={n_max} n_max={n_max} step={n_step} sup={self.sup:.2f}')
        approx: typ.Dict[int, FloatArray] = {}
        for n in range(n_min, n_max+n_step, n_step):
            approx[n] = self._calc(n, k, self.gen)
        return approx

    def _calc(self, n: int, k: int, gen: np.random.Generator) -> FloatArray:
        results: FloatArray = np.zeros(k, dtype=np.float64)
        for i in range(k):
            c = np.count_nonzero(
                self.checkFn(
                    self.fn, (
                        gen.random((n,)) * (self.b - self.a) + self.a,
                        gen.random((n,)) * self.sup
                    )))
            results[i] = (c/n*(self.b-self.a)*self.sup)
        return results


def run_experiment(
        mci: MonteCarloIntegral, k: int, expected: float, 
        title: str | None = None, filename: str | None = None, 
        transform: None | TransformType = None, showplt: bool = True):
    result = mci.go(k)
    y, x = mci.as_plt(k, result)

    meany = np.mean(np.array([arr for arr in result.values()]), axis=1)
    meanx = mci.n_range()

    if transform != None:
        meany = transform(meany)
        y = transform(y)

    fig, ax = plt.subplots()

    ax.scatter(x, y, s=5, color="#38B0E8")
    ax.scatter(meanx, meany, s=20, color="#BA23D3")
    ax.plot([meanx[0], meanx[len(meanx)-1]],
             [expected, expected], color="#D3BA23", linewidth=2)

    if title:
        fig.suptitle(f'{title} k={k}')
    ax.xaxis.set_label('N')
    ax.yaxis.set_label('Approx Value')
    ax.grid(True)

    if filename != None:
        fig.savefig(filename, dpi=600)

    if showplt:
        plt.show()

    plt.close(fig)


FUNCS = ('x3', 'cbrt', 'sin', 'poly', 'pi')


def get_args() -> arg.Namespace:
    p = arg.ArgumentParser('Monte Carlo Integration')
    p.add_argument('-k', default=5, type=int,
                   help='Number of iterations for each n')
    p.add_argument(
        '-f', '--func', default=FUNCS[0], choices=FUNCS, type=str, help='Function to integrate')
    p.add_argument('-s', '--filename', default=None, type=str,
                   help='Filename for the saved graph')
    p.add_argument('-t', '--title', default='Monte Carlo Integration', type=str, help='Set the title of the plot')
    
    p.add_argument('-r', '--run_all', action='store_true', default=False, 
                    help='Run all experiments metioned in the exercise (poly, sin, cbrt, pi) with k=[5, 50]')
    return p.parse_args()


def main():
    args = get_args()

    # Create vectorized versions of the integration functions
    def vectorized_poly(x: VectorVType) -> VectorVType:
        return 4*x*(x-1)**3

    # No actual function for the half of circle, because
    # I'm using the `is_in_cricle` custom hook
    # for checking if the point is inside the circle

    def dummy_circle(x: VectorVType) -> VectorVType:
        return 0.0

    # Simply check if given 2 points are inside the unit circle
    def is_in_circle(f: VectorizedFn, v: FnCheckVArg) -> Union[bool, BoolArray]:
        x, y = v
        return (x ** 2 + y**2) <= 1

    def transform_circle(p: FloatArray) -> FloatArray:
        return p * 2

    # MonteCarloIntegral: function, a, b, [checkFn]
    integrals = (
        (MonteCarloIntegral(lambda x: x**3, 1, 3), 20.0, None, 'x^3'),
        (MonteCarloIntegral(lambda x: np.cbrt(x), 0, 8), 12.0, None, 'Cbrt'),
        (MonteCarloIntegral(lambda x: np.sin(x), 0, np.pi), 2.0, None, 'Sin'),
        (MonteCarloIntegral(vectorized_poly, 0, 1), -0.2, None, 'Poly'),
        (MonteCarloIntegral(dummy_circle, -1, 1, 1, is_in_circle), np.pi, transform_circle, 'Pi')
    )

    print(args._get_kwargs())

    if not args.run_all:
        mci, expected, transform, _ = integrals[FUNCS.index(args.func)]
        run_experiment(mci, args.k, expected, args.title, args.filename, transform)
    else:
        KS = [5, 50]
        print('Running all experiments with k={}'.format(KS))

        for k in KS:
            for mci, expected, transform, title in integrals:
                run_experiment(mci, k, expected, None, f'{title}-{k}.png', transform, showplt=False)

if __name__ == '__main__':
    main()
