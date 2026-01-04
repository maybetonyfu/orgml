#!/bin/bash
# Test runner for orgml.el unit tests

echo "Running orgml.el unit tests..."
echo "=============================="

# Run tests in batch mode
emacs --batch -l orgml.el -l test-orgml.el -f ert-run-tests-batch-and-exit

# Check exit code
if [ $? -eq 0 ]; then
    echo ""
    echo "✅ All tests passed!"
else
    echo ""
    echo "❌ Some tests failed!"
    exit 1
fi