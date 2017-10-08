#include <iostream>
#include <vector>

using namespace std;
using array = vector < double >;
using matrix = vector < array >;
using uint = unsigned;
const auto mu = -9.0;
const uint n = 4;
const auto eps = 1e-8;
const uint MAX_COUNT = 10000;

array operator*(const matrix& lhs, const array& rhs)
{
	if (lhs[0].size() != rhs.size())
		return array();
	array ans(lhs.size(), 0.0);
	for (uint i = 0; i < ans.size(); ++i)
	{
		for (uint j = 0; j < rhs.size(); ++j)
		{
			ans[i] += lhs[i][j] * rhs[j];
		}
	}
	return ans;
}
array operator+(const array& lhs, const array& rhs)
{
	if (lhs.size() != rhs.size())
		return array();
	array ans(lhs.size(), 0.0);
	for (uint i = 0; i < ans.size(); ++i)
	{
		ans[i] = lhs[i] + rhs[i];
	}
	return ans;
}
double diff(const array& lhs, const array& rhs)
{
	if (lhs.size() != rhs.size())
		return INFINITY;
	double sum = 0.0;
	for (uint i = 0; i < lhs.size(); ++i)
	{
		sum += (abs(lhs[i] - rhs[i]));
	}
	return sum;
}
int main()
{
	matrix A{
		{ -0.95, -0.06, -0.12, 0.14 },
		{ 0.04, -1.12, 0.08, 0.11 },
		{ 0.34, 0.08, -1.06, 0.14 },
		{ 0.11, 0.12, 0.00, -1.03 }};
	array g{ 2.17, -1.40, 2.10, 0.80 };
	matrix B(n, array(n));

	for (uint i = 0; i < n; ++i)
	{
		for (uint j = 0; j < n; ++j)
		{
			B[i][j] = ((i == j) ? 1.0 : 0.0) - mu*A[i][j];
		}
	}
	for (uint j = 0; j < n; ++j)
	{
		g[j] *= mu;
	}
	array x(n, 0.0);
	array x1(n, 0.0);
	uint count = 0;
	do
	{
		x = x1;
		x1 = B * x + g;
	} while ((diff(x, x1) > eps) && (count++ < MAX_COUNT));
	cout << "Number of iterations:";
	cout << count << endl;
	cout << "Solution:\n";
	for (uint i = 0; i < x1.size(); ++i)
	{
		cout << x1[i] << endl;
	}
	return 0;
}

