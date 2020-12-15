import java.util.concurrent.Semaphore;

//Mateusz Grzesiuk

//Zadanie 2

class IntCell {
    private int n = 0;
    public int getN() {return n;}
    public void setN(int n) {this.n = n;}
}

//A)

class CountA extends Thread {
    private static IntCell n = new IntCell();
    @Override public void run() {
        int temp;
        for (int i = 0; i < 200000; i++) {
            synchronized(n){
                temp = n.getN();
                n.setN(temp + 1);
            }
        }
    }
    public static void main(String[] args) {
        CountA p = new CountA();
        CountA q = new CountA();
        p.start();
        q.start();
        try { p.join(); q.join(); }
        catch (InterruptedException e) { }
        System.out.println("The value of n is " + n.getN());
    }
}

//B)
class CountB extends Thread {
    private static IntCell n = new IntCell();
    private static Semaphore s = new Semaphore(1);
    @Override public void run() {
        int temp;
        for (int i = 0; i < 200000; i++) {
            while(!s.tryAcquire()){}
            temp = n.getN();
            n.setN(temp + 1);
            s.release();
        }
    }
    public static void main(String[] args) {
        CountB p = new CountB();
        CountB q = new CountB();
        p.start();
        q.start();
        try { p.join(); q.join(); }
        catch (InterruptedException e) { }
        System.out.println("The value of n is " + n.getN());
    }
}
