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
  async function handleError(e:Error) {
    if (e instanceof IntentionalStopError) {
      // Simulate recovery. In this case, we're just continuing from the `.s` word
    }
    else {
      throw e
    }
  }

  await interp.run("1 .s 2 .s +", { handleError });
  expect(interp.get_stack()).toEqual([3]);
});


test("Simulate recovery", async () => {
  async function handleError(e:Error) {
    if (e instanceof UnknownWordError) {
      // Simulate recovery
      interp.stack_push(2);
    }
    else {
      throw e
    }
  }

  await interp.run("1 garbage +", { handleError, maxAttempts: 3 });
  expect(interp.get_stack()).toEqual([3]);
});


test("Simulate multiple recoveries", async () => {
  // Define error handler
  async function handleError(e:Error) {
    if (e instanceof UnknownWordError) {
      // Simulate recovery
      interp.stack_push(2);
    }
    else {
      throw e
    }
  }

  // Set up a case with multiple errors
  const num_attempts = await interp.run("1 garbage + more_garbage +", { handleError, maxAttempts: 3 });
  expect(interp.get_stack()).toEqual([5]);
  expect(num_attempts).toEqual(3);
});


test("Simulate correction", async () => {
  async function handleError(e:Error) {
    if (e instanceof UnresolvedRecipientsError) {
      // Simulate correction
      interp.stack_push(simulate_email_correction(e.get_email()));
    }
    else {
      throw e
    }
  }

  const invalid_email = {
    recipients: [["John Parker", "John Whorfin", "John Bigboote"]],
    body: "We're gonna go real soon!"
  };
  interp.stack_push(invalid_email);
  await interp.run("VALIDATE-EMAIL SEND-EMAIL", { handleError });
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
