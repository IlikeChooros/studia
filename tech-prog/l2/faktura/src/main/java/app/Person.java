package app;

public class Person implements DataLike {

    /** First name. */
    private String firstName;
    /** Last name. */
    private String lastName;
    /** ID number (PESEL). */
    private String idNumber;

    /**
     * Constructor for Person class.
     *
     * @param f The first name of the person.
     * @param l The last name of the person.
     * @param id The identification number of the person.
     */
    public Person(final String f,
        final String l, final String id) {
        this.firstName = f;
        this.lastName = l;
        this.idNumber = id;
    }

    /**
     * Get the name of the person.
     * @return The full name of the person.
     */
    public String getName() {
        return firstName + " " + lastName;
    }

    /**
     * Get the id number of the person.
     * @return The identification number of the person (pesel).
     */
    public String getIdNumber() {
        return idNumber;
    }
}
