package ssgr;

public class InputStream {

    private String str;
    private int length;
    public int next = 0;
    private int mark = 0;

    public InputStream(String s) {
        this.str = s;
        this.length = s.length();
    }

    public int read() {
        if (next >= length)
            return -1;
        return str.charAt(next++);
    }
    
    public long skip(long n) {
        if (next >= length)
                return 0;
        // Bound skip by beginning and end of the source
        long r = Math.min(length - next, n);
        r = Math.max(-next, r);
        next += r;
        return r;
    }

}
