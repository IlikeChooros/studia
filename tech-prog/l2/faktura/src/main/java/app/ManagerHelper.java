package app;

import java.util.Vector;

public class ManagerHelper<T extends DataLike> implements ManagerLike<T> {
    /** Collection of data objects to manage. */
    protected Vector<T> collection;

    /** Maximum number of search results to return. */
    public static final int MAX_SEARCH_RESULTS = 10;

    /**
     * Constructs a ManagerHelper with the given collection.
     * @param coll The collection to manage
     */
    public ManagerHelper(final Vector<T> coll) {
        this.collection = coll;
    }

    /**
     * Constructs an empty ManagerHelper.
     */
    public ManagerHelper() {
        this.collection = new Vector<>();
    }

    /** Adds an item to the collection.
     * @param item The item to add
     */
    public void add(final T item) {
        collection.add(item);
    }

    /** Returns all items in the collection.
     * @return The collection of items
     */
    public Vector<T> getAll() {
        return collection;
    }

    /** Searches the collection for items matching the query.
     * @param query The search query
     * @return A vector of matching items
     */
    public Vector<T> search(final String query) {
        Vector<T> results = new Vector<>();
        for (T item : collection) {
            if (item.toString().toLowerCase()
                .contains(query.toLowerCase())) {
                results.add(item);

                if (results.size() >= MAX_SEARCH_RESULTS) {
                    break;
                }
            }
        }
        return results;
    }


    /** Removes an item from a collection.
     * @param item The item to remove
     */
    public void remove(final T item) {
        collection.remove(item);
    }

    /** Gets an item by its ID.
     * @param id The ID of the item
     * @return The item with the specified ID, or null if not found
     */
    public T getById(final int id) {
        if (id < 0 || id >= collection.size()) {
            return null;
        }
        return collection.get(id);
    }
}
