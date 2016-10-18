#include <iostream>
#include <vector>
using namespace std;

using array = vector < double >;
using matrix = vector < array >;

const int n = 4;
const double eps = 1e-8;

double solve(double b)
{
	return (sqrt(b*b + 4.0) - b) / 2.0;
}

matrix operator*(matrix lhs,matrix rhs)
{
	if (lhs[0].size() != rhs.size())
		return matrix();
	int ans_n = lhs.size();
	int ans_m = rhs[0].size();
	matrix ans(ans_n, array(ans_m));
	for (int i = 0; i < ans_n; ++i)
	{
		for (int j = 0; j < ans_m; ++j)
		{
			for (int k = 0; k < rhs.size(); ++k)
			{
				ans[i][j] += lhs[i][k] * rhs[k][j];
			}
		}
	}
	return ans;
}

matrix transpose(matrix val)
{
	matrix ans(val);
	for (int i = 0; i < val.size(); ++i)
	{
		for (int j = i + 1; j < val[0].size(); ++j)
		{
			swap(ans[i][j], ans[j][i]);
		}
	}
	return ans;
}

void print(matrix val)
{
	for (int i = 0; i < val.size(); ++i)
	{
		for (int j = 0; j < val[0].size(); ++j)
		{
			cout << val[i][j] << " \t";
		}
		cout << endl;
	}
}

pair<int,int> find_elem(matrix val)
{
	int x = 0, y = 0;
	double max = 0.0;
	for (int i = 0; i < val.size(); ++i)
	{
		for (int j = i + 1; j < val[0].size(); ++j)
		{
			if (abs(val[i][j]) > max)
			{
				x = i;
				y = j;
				max = abs(val[i][j]);
			}
		}
	}
	if (max > eps)
		return make_pair(x, y);
	else
		return make_pair(0, 0);
}

int main()
{
	//matrix A(n, vector<double>(n));
	matrix A = {
		{ 2.0, 1.2, -1.0, 1.0 },
		{ 1.2, 0.5, 2.0, -1.0 },
		{ -1.0, 2.0, -1.5, 0.2 },
		{ 1.0, -1.0, 0.2, 1.5 } };
	matrix Identity(n, array(n));
	for (int i = 0; i < n; ++i)
		Identity[i][i] = 1.0;
	matrix S(Identity);
	cout.precision(10);
	int count = 0;
	for (pair<int, int> pos = find_elem(A); ((pos.first != 0) || (pos.second != 0)); pos = find_elem(A), count++)
	{
		int p = pos.first, q = pos.second;
		double theta = (A[q][q] - A[p][p]) / A[p][q];
		double t = solve(theta);
		double c = 1.0 / sqrt(t*t + 1.0);
		double s = c * t;
		matrix P(Identity);
		P[p][p] = c;
		P[q][q] = c;
		P[p][q] = s;
		P[q][p] = -s;
		matrix P_transpose(P);
		swap(P_transpose[p][q], P_transpose[q][p]);
		A = P_transpose * A * P;
		S = S * P;
	}
	cout << "Count of iterations:";
	cout << count << endl;
	cout << "Eigenvalues:\n";
	for (int i = 0; i < n; ++i)
	{
		cout << A[i][i] << endl;
	}
	cout << "Eigenvectors:\n";
	for (int i = 0; i < n; ++i)
	{
		cout << '{';
		for (int j = 0; j < n; ++j)
		{
			cout << S[j][i] << ", ";
		}
		cout << "};" << endl;
	}
	return 0;
}