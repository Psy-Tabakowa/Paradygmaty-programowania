//Mateusz Grzesiuk

//Zadanie 4

import java.util.concurrent.Semaphore;

class Philosoph extends Thread {
    public static Semaphore[] chopsticks = new Semaphore[5];
    public static Semaphore doorman = new Semaphore(4);
    public static int index = 0;
    public int number;

    public Philosoph(int number){
        this.number=number;
    }

    public static void main(String[] args){
        Philosoph[] philosophs = new Philosoph[5];
        for(int i = 0; i<5; i++) {
            chopsticks[i] = new Semaphore(1);
            philosophs[i] = new Philosoph(i+1);
        }
        for(int i = 0; i<5; i++)
            philosophs[i].start();

        try{
            for(int i = 0; i<5; i++)
                philosophs[i].join();
        }
        catch (InterruptedException e) { }
    }

    @Override public void run() {
        for(int j=0; j<7; j++){
            System.out.println(number + ". Meditating");
            for(int i=0; i<100; i++){}
            System.out.println(number + ". Went to the kitchen");
            goEat();
        }
    }

    private void goEat(){
        int chopstick;
        while(!doorman.tryAcquire()){ }
        System.out.println(number + ". Entered the kitchen");
        while(!chopsticks[index].tryAcquire()){index=(index+1)%5;}
        chopstick=index;
        System.out.println(number + ". Grabbed left chopstick " + chopstick);
        while(!chopsticks[(chopstick+1)%5].tryAcquire()){}
        System.out.println(number + ". Grabbed right chopstick " + (chopstick+1)%5);

        index=(index+2)%5;
        eat();

        System.out.println(number + ". Released left chopstick " + chopstick);
        chopsticks[chopstick].release();
        System.out.println(number + ". Released right chopstick " + (chopstick+1)%5);
        chopsticks[(chopstick+1)%5].release();
        System.out.println(number + ". Left Kitchen");
        doorman.release();
    }

    private void eat(){
        System.out.println(number + ". Started eating");
        for(int i=0; i<100; i++){ }
        System.out.println(number + ". Ended eating");
    }

}
