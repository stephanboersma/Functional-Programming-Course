class test {
    public int[] A = {9,8,7,6,5,4,3,2,1,0};
    public int c = 0;
    public void C(int j) {
        if (j == 0) {
            return;
        }
        if (A[j - 1] > A[j]) {
            System.out.print("swap: " + A[j] + " and " + A[j-1]);
            
            int temp = A[j - 1];
            A[j-1] = A[j];
            A[j] = temp;
            System.out.print(" Resulting in [");
            for (int s : A) {
                System.out.print(s + ", ");
            }
            System.out.println("]");
            C(j - 1);
        } 
    }
    public static void main(String[] args) {
        test t = new test();

        for (int i = 1; i <t.A.length; i++) {
            t.C(i);
        }
       // for (int s : t.A) System.out.println(s);

    }
}