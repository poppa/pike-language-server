// "Dummy" file to test how Pike actually handles various intricate
// syntax things.

public string constant name = "Pike",
  domain = "General Purpose",
  creator = "Fredrik Hübinette";

__attribute__("test") __attribute__("main") int main() {
  werror("Name: %O\n", name);
}
