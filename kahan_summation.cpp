#include <iostream>
#include <vector>

using namespace std;

int main() {

}

double kahan_sum(vector<double>& seq) {
    double sum = 0.0, c = 0.0;
    double y, t;
    for (double k : seq) {
        y = k - c;
        t = sum + y;
        c = (t - sum) - y;
        sum = t;
    }
    return sum;
}