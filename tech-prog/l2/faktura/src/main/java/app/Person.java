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

    @Override
    public final String toString() {
        return String.format("%s %s, PESEL: %s",
            firstName, lastName, idNumber);
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

    @Override
    public final java.util.Map<String, String> getAllFieldNames() {
        java.util.Map<String, String> fieldMap
            = new java.util.LinkedHashMap<>();
        fieldMap.put("firstName", "First Name");
        fieldMap.put("lastName", "Last Name");
        fieldMap.put("idNumber", "ID Number (PESEL)");
        return fieldMap;
    }

    @Override
    public final boolean updateField(final String field,
        final Object... values) {
        if (values.length == 0) {
            return false;
        }

        switch (field) {
            case "firstName":
                if (values[0].getClass() != String.class) {
                    return false;
                }
                this.firstName = (String) values[0];
                return true;
            case "lastName":
                if (values[0].getClass() != String.class) {
                    return false;
                }
                this.lastName = (String) values[0];
                return true;
            case "idNumber":
                if (values[0].getClass() != String.class) {
                    return false;
                }
                this.idNumber = (String) values[0];
                return true;
            default:
                return false;
        }
    }

    @Override
    public final Object getValue(final String fieldName) {
        switch (fieldName) {
            case "firstName":
                return firstName;
            case "lastName":
                return lastName;
            case "idNumber":
                return idNumber;
            default:
                return null;
        }
    }
}
