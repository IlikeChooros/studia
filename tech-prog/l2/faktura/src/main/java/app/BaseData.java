package app;

public abstract class BaseData {

    /** ID counter for generating unique IDs. */
    private static int idCounter = 0;

    /** Unique ID for the data object. */
    protected int id;

    /** Constructor for DataLike class. */
    public BaseData() {
        this.id = idCounter++;
    }

    /** Get the unique ID of the data object.
     * @return The unique ID.
     */
    public int getId() {
        return id;
    }

    // Simply return all field names
    // public String[] GetAllFieldNames();

    // // May throw some kind of exception if invalid
    // public void UpdateField(String field, Object... values);
}
