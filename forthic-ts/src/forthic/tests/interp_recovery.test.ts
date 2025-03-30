import { Interpreter } from "../interpreter";
import { IntentionalStopError } from "../errors";
import { UnknownWordError, WordExecutionError } from "../errors";
import { Module } from "../module";

let interp = null;

// Setup
beforeEach(async () => {
  interp = new Interpreter();
  interp.register_module(new SampleModule(interp));
  await interp.run(`[ ["sample" ""] ] USE-MODULES`);
});

test("Continue from `.s`", async () => {
  async function handleError(e: Error, _interp: Interpreter) {
    if (e instanceof IntentionalStopError) {
      // Simulate recovery. In this case, we're just continuing from the `.s` word
    } else {
      throw e;
    }
  }

  interp.set_error_handler(handleError);
  await interp.run("1 .s 2 .s +");
  expect(interp.get_stack().get_items()).toEqual([3]);
});

test("Continue from `.s` with intervening call", async () => {
  async function handleError(e: Error, _interp: Interpreter) {
    if (e instanceof IntentionalStopError) {
      // Simulate recovery. In this case, we're just continuing from the `.s` word
      await interp.run("");
    } else {
      throw e;
    }
  }

  interp.set_error_handler(handleError);
  await interp.run("1 .S 2 .S +");
  expect(interp.get_stack().get_items()).toEqual([3]);
});

test("Simulate recovery", async () => {
  async function handleError(e: Error, interp: Interpreter) {
    if (e instanceof UnknownWordError) {
      // Simulate recovery
      interp.stack_push(2);
    } else {
      throw e;
    }
  }

  interp.set_max_attempts(3);
  interp.set_error_handler(handleError);
  await interp.run("1 garbage +");
  expect(interp.get_stack().get_items()).toEqual([3]);
});

test("Simulate multiple recoveries", async () => {
  // Define error handler
  async function handleError(e: Error, interp: Interpreter) {
    if (e instanceof UnknownWordError) {
      // Simulate recovery
      interp.stack_push(2);
    } else {
      throw e;
    }
  }

  // Set up a case with multiple errors
  interp.set_max_attempts(3);
  interp.set_error_handler(handleError);
  await interp.run("1 garbage + more_garbage +");
  expect(interp.get_stack().get_items()).toEqual([5]);
});

test("Simulate correction", async () => {
  async function handleError(e: Error, interp: Interpreter) {
    if (e instanceof WordExecutionError) {
      if (e.getRootError() instanceof UnresolvedRecipientsError) {
        const unresolved_recipients_error =
          e.getRootError() as UnresolvedRecipientsError;
        // Simulate correction
        interp.stack_push(
          simulate_email_correction(unresolved_recipients_error.get_email()),
        );
      }
    } else {
      throw e;
    }
  }

  const invalid_email = {
    recipients: [["John Parker", "John Whorfin", "John Bigboote"]],
    body: "We're gonna go real soon!",
  };
  interp.stack_push(invalid_email);

  interp.set_error_handler(handleError);
  await interp.run("VALIDATE-EMAIL SEND-EMAIL");
});

// Simulate a correction
//
// This would correct anything that was invalid about the email
function simulate_email_correction(email: any) {
  email.recipients = [email.recipients.map((recipient: any[]) => recipient[0])];
  return email;
}

// ==========================
// Sample module
// ==========================
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

    interp.stack_push(email);
  }

  // ( email -- )
  async word_SEND_EMAIL() {
    const email = interp.stack_pop();
    console.log("Sending email", email);
  }
}
