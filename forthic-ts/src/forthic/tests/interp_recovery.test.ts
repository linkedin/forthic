import { Interpreter } from "../interpreter";
import { IntentionalStopError } from "../errors";
import { UnknownWordError } from "../errors";
import { Module } from "../module";

let interp = null;

// Setup
beforeEach(async () => {
  interp = new Interpreter();
  interp.register_module(new SampleModule(interp));
  await interp.run(`[ ["sample" ""] ] USE-MODULES`)
});

test("Continue from `.s`", async () => {
  try {
    await interp.run("1 2 .s +");
  }
  catch (e) {
    if (e instanceof IntentionalStopError) {
      await interp.continue();
      expect(interp.get_stack()).toEqual([3]);
    }
  }
});


test("Simulate recovery", async () => {
  try {
    await interp.run("1 garbage +");
  }
  catch (e) {
    if (e instanceof UnknownWordError) {
      // Simulate recovery
      interp.stack_push(2);
      await interp.continue();
      expect(interp.get_stack()).toEqual([3]);
    }
    else {
      throw e
    }
  }
});


test("Simulate multiple recoveries", async () => {
  // Define error handler
  async function handle_error(e:Error, run:RunFunction, numAttempts:number) {
    if (e instanceof UnknownWordError) {
      // Simulate recovery
      interp.stack_push(2);
      return await run(interp, handle_error, numAttempts);
    }
    else {
      throw e
    }
  }

  // Set up a case with multiple errors
  interp.prime("1 garbage + more_garbage +");
  const num_attempts = await run_handling_errors(interp, handle_error);
  expect(interp.get_stack()).toEqual([5]);
  expect(num_attempts).toEqual(3);
});


test("Simulate correction", async () => {
  try {
    const invalid_email = {
      recipients: [["John Parker", "John Whorfin", "John Bigboote"]],
      body: "We're gonna go real soon!"
    };
    interp.stack_push(invalid_email);
    await interp.run("VALIDATE-EMAIL SEND-EMAIL");
  }
  catch (e) {
    if (e instanceof UnresolvedRecipientsError) {
      const email = e.get_email();
      console.log("Caught error", email.recipients);

      // Simulate correction
      console.log("Correcting error...");
      const corrected_email = simulate_email_correction(email);
      interp.stack_push(corrected_email);
      await interp.continue();
    }
    else {
      throw e
    }
  }
});

// Simulate a correction
//
// This would correct anything that was invalid about the email
function simulate_email_correction(email: any) {
  email.recipients = [email.recipients.map((recipient: any[]) => recipient[0])];
  return email;
}



// Sample module
class UnresolvedRecipientsError extends Error {
  constructor(private email: any) {
    super("Email recipients must be an array of resolved contacts");
  }

  get_email() {
    return this.email;
  }
}

class SampleModule extends Module {
  constructor(interp: Interpreter) {
    super("sample", interp);
    this.add_module_word("VALIDATE-EMAIL", this.word_VALIDATE_EMAIL.bind(this));
    this.add_module_word("SEND-EMAIL", this.word_SEND_EMAIL.bind(this));
  }

  // ( email -- email )
  async word_VALIDATE_EMAIL() {
    const email = interp.stack_pop();
    if (email.recipients.some((recipient: any[]) => recipient.length > 1)) {
      throw new UnresolvedRecipientsError(email);
    }
    else {
      interp.stack_push(email);
    }
  }

  // ( email -- )
  async word_SEND_EMAIL() {
    const email = interp.stack_pop();
    console.log("Sending email", email);
  }
}



// -----------------------------------------------------------------------------------------------
// Error handling support
const MAX_ATTEMPTS = 10;

type RunFunction = (interpreter: Interpreter, handleError:HandleErrorFunction, numAttempts:number) => Promise<number>;
type HandleErrorFunction = (e: Error, run: RunFunction, numAttempts:number) => Promise<number>;

async function run_handling_errors(interpreter: Interpreter, handleError:HandleErrorFunction, numAttempts:number=0) {
  try {
    numAttempts++;
    if (numAttempts > MAX_ATTEMPTS) {
      throw new Error("Too many attempts");
    }
    await interpreter.continue();
    return numAttempts;
  }
  catch (e) {
    return await handleError(e, run_handling_errors, numAttempts);
  }
}
