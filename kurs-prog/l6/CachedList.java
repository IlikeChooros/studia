
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
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
 * A generic, cache-friendly list implementation that pre-allocates and reuses objects of type {@code E}.
 * <p>
 * {@code CachedList} is designed to minimize object allocations by maintaining a pool of reusable items.
 * It requires that {@code E} extends {@link CachableItem} and provides a public no-argument constructor.
 * Items are initialized up to the specified capacity and reused as needed.
 * </p>
 *
 * @param <E> the type of elements in this list, which must extend {@link CachableItem}
 *
 * <p><b>Features:</b></p>
 * <ul>
 *   <li>Pre-allocates objects to reduce allocation overhead.</li>
 *   <li>Supports adding elements by copying or by setting fields via varargs.</li>
 *   <li>Implements {@link CachableList} interface.</li>
 *   <li>Provides an iterator that only iterates over valid (added) elements.</li>
 *   <li>Allows clearing and resetting the list without deallocating objects.</li>
 * </ul>
 *
 * <p><b>Usage Notes:</b></p>
 * <ul>
 *   <li>Ensure that {@code E} has a public no-argument constructor.</li>
 *   <li>Use {@link #add(Object...)} to set fields of a new element, or {@link #add(Object)} to copy an existing one.</li>
 *   <li>Call {@link #clear()} to reset the list for reuse.</li>
 * </ul>
 *
 * @see CachableItem
 * @see CachableList
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
