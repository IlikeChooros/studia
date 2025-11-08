package app;

import java.util.Vector;

public class PersonManager extends ManagerHelper<Person> {
    PersonManager(final Vector<Person> coll) {
        super(coll);
    }
}
