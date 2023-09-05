int AX = 0;
int sum(int n) {
    int s = 0;
    int x = 1;
    for (int i = 1; i <= n; i++) 
    {
        int y = i + 1;
        s += y;
        x -= i;
    }
    int i = 5;
    s += 5;
    return s;
}

int main() {
    int ans = sum(10);
    return 0;
}