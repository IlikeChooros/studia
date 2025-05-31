
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.Iterator;
import java.util.Vector;


interface CachableItem<E> {
    public void setAllFields(Object... items);
    public void copy(E other);
}

interface CachableList<E extends CachableItem<E>> extends Iterable<E> {

    public void clear();
    public int size();
    public boolean isEmpty();
    public E firstElement();
    public E get(int i);
    public void add(E elem);
    public void setAll(CachableList<E> other);
}


/**
 * Optimized vector for non-copy assigments. 
 */
public class CachedList<E extends CachableItem<E>> implements CachableList<E> {
    private final Vector<E> items;
    private final Constructor<E> constructorE;
    private int size;

    private void pushNewItems(int count) {
        for (int i = 0; (i < count) && (constructorE != null); i++) {
            try {
                this.items.add(constructorE.newInstance());
            } catch (InstantiationException | IllegalAccessException 
                | IllegalArgumentException | InvocationTargetException e) {
                
                e.printStackTrace();
            }
        }
    } 


    public CachedList(Class<E> clazz, int capacity)
    {
        Constructor<E> constE = null;
        this.items = new Vector<>(capacity);
        this.size = 0;
        
        try {
            constE = clazz.getConstructor();
        } catch (NoSuchMethodException | SecurityException e) {
            constE = null;
            System.err.println("CachableItem<E> must have an empty constructor avaiable");
            e.printStackTrace();
        }
        finally {
            constructorE = constE;
        }

        pushNewItems(capacity);
    }

    public void add(Object... args)
    {
        if (size == this.items.size()) {
            pushNewItems(size);
        }

        this.items.get(size).setAllFields(args);
        size++;
    }

    public void add(E obj) {
        if (size == this.items.size()) {
            pushNewItems(size);
        }
        
        this.items.get(size).copy(obj);
        size++;
    }

    public void clear() {
        this.size = 0;
    }

    public int size() {
        return this.size;
    }

    public void setAll(CachableList<E> other) {
        // 'Copy' all moves into this list
        this.size = 0;
        for (E m : other) {
            add(m);
        }
    }

    public boolean isEmpty() {
        return this.size == 0;
    }
    
    public E firstElement() {
        return this.items.firstElement();
    }

    public E get(int i) {
        return this.items.get(i);
    }

    /**
     * Overrides the vectors iterator, to access only the
     * items we actually specified in 'add'
     */
    @Override
    public Iterator<E> iterator() {
        return new Iterator<E>() {
            private int currentIdx = 0;

            @Override
            public boolean hasNext() {
                this.currentIdx++;
                return (this.currentIdx - 1) < size;
            }

            @Override
            public E next() {
                return items.get(this.currentIdx - 1);
            }
        };
    }
}
