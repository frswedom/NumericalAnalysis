#include <iostream>
#include <fstream>
#include <cmath>
#include <vector>
#include <complex>

using namespace std;
using base = complex<double>;
using list = vector<double>;
using matrix = vector<list>;
using uint = size_t;
const double pi = 3.1415926535897932384626433832795;
const double e = 2.718281828459045;
const uint N = 1024;

double f(double t)
{
	return -10 * sin(pi * t / 18) + 5 * sin(pi * t / 80); // T1 = 36  T2 = 160
}

double f2(double t)
{
	return -10 * sin(pi*t / 18)*pow(e, -t * t /2) + 5 * sin(pi*t / 80); // T1 = 36  T2 = 160
}

double f3(double t)
{
    return sin(pi*t / 18)*log2(t * t /2);
}

double f4(double t)
{
    return pow(sin(pi*t), 3)*pow(e, -t * t);
}
void fft(vector<base> & a, bool invert) {
	int n = (int)a.size();

	for (int i = 1, j = 0; i < n; ++i) {
		int bit = n >> 1;
		for (; j >= bit; bit >>= 1)
			j -= bit;
		j += bit;
		if (i < j)
			swap(a[i], a[j]);
	}

	for (int len = 2; len <= n; len <<= 1) {
		double ang = 2 * pi / len * (invert ? -1 : 1);
		base wlen(cos(ang), sin(ang));
		for (int i = 0; i < n; i += len) {
			base w(1);
			for (int j = 0; j < len / 2; ++j) {
				base u = a[i + j], v = a[i + j + len / 2] * w;
				a[i + j] = u + v;
				a[i + j + len / 2] = u - v;
				w *= wlen;
			}
		}
	}
	if (invert)
		for (int i = 0; i < n; ++i)
			a[i] /= n;
}

int main()
{
	vector<base> r(N);

	//average of function
	double avg_f = 0;
	for (uint i = 0; i < N; ++i)
	{
		avg_f += f(i);
	}
	avg_f /= N;

	//evaluate covariation function
	for (uint j = 0; j < N; ++j)
	{
		for (uint k = 0; k < (N - j); k++)
		{
			r[j] += (f(j + k) - avg_f) * (f(k) - avg_f);
		}
	}
	for (uint j = 1; j < N; ++j)
	{
		r[j] /= r[0];
	}
	r[0] = 1;

	ofstream out_raw("out_raw.txt");
	for (int i = 0; i < N; i++)
	{
		out_raw << r[i].real() << '\n';
	}
	out_raw.close();
	//
	fft(r, false);

	ofstream out("out.txt");

	for (int i = 0; i < N; i++)
	{
		out << sqrt(r[i].real()*r[i].real() + r[i].imag()*r[i].imag()) << '\n';
	}
	out.close();
	return 0;
}

