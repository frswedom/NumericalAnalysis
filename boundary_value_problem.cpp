#include <fstream>
#include <math.h>
#include <vector>

using _list = std::vector<double>;
using uint = size_t;
using namespace std;

const double minValue = 1.3;
const double maxValue = 2.1;
const int N = 400;
const double h = (maxValue - minValue) / N;

void solve_tridiagonal(const _list &a, const _list &b, _list &c, _list &x)
{
	auto X = a.size();

	c[0] = c[0] / b[0];
	x[0] = x[0] / b[0];
	double m;
	/* loop from 1 to X - 1 inclusive, performing the forward sweep */
	for (uint ix = 1; ix < X; ix++)
	{
		m = (b[ix] - a[ix] * c[ix - 1]);
		c[ix] = c[ix] / m;
		x[ix] = (x[ix] - a[ix] * x[ix - 1]) / m;
	}

	/* loop from X - 2 to 0 inclusive (safely testing loop condition for an unsigned integer), to perform the back substitution */
	for (uint ix = X - 1; ix-- > 0; )
		x[ix] = x[ix] - c[ix] * x[ix + 1];
}

int main()
{
	_list a(N + 1), b(N + 1), c(N + 1), y(N + 1);
	for (uint i = 1; i < N; ++i)
	{
		a[i] = 1 - 0.4*h;
		b[i] = -2 - h*h*(minValue + h*i);
		c[i] = 1 + 0.4*h;
		y[i] = 1.4*h*h;
	}
	//boundary conditions
	b[0] = 1; c[0] = 0; y[0] = 0.5;
	a[N] = -1; b[N] = 2 * h + 1; y[N] = 1.7*h;

	solve_tridiagonal(a, b, c, y);
	ofstream out("out.txt");
	out.precision(15);
	for (uint i = 0; i <= N; ++i)
	{
		out << (minValue + h*i) << ' ' << y[i] << endl;
	}
	out.close();
    return 0;
}

