import java.lang.reflect.Method;

public class run_test {
    public static void main(String[] args)
    {
        try {
            // Load the TestH5A class
            Class<?> testClass  = Class.forName("test.TestH5A");
            Object testInstance = testClass.getDeclaredConstructor().newInstance();

            // Find and invoke the testH5AVLwr method
            Method testMethod = testClass.getMethod("testH5AVLwr");
            System.out.println("Running testH5AVLwr...");
            testMethod.invoke(testInstance);
            System.out.println("Test completed successfully!");
        }
        catch (Exception e) {
            System.err.println("Test failed: " + e.getMessage());
            e.printStackTrace();
        }
    }
}