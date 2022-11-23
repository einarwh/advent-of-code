using System;
using System.IO;
using System.Linq;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;

namespace Aoc18Antlr
{
    class Program
    {
        private static long Rules1(string s)
        {
            var charStream = new AntlrInputStream(s);
            var lexer = new Exp1Lexer(charStream);
            var tokenStream = new CommonTokenStream(lexer);
            var parser = new Exp1Parser(tokenStream);
            var tree = parser.expression();
            return tree.Accept(new Exp1Visitor());
        }

        private static long Rules2(string s)
        {
            var charStream = new AntlrInputStream(s);
            var lexer = new Exp2Lexer(charStream);
            var tokenStream = new CommonTokenStream(lexer);
            var parser = new Exp2Parser(tokenStream);
            var tree = parser.expression();
            return tree.Accept(new Exp2Visitor());
        }

        private static long Run(string[] lines, Func<string, long> evaluator)
        {
            return lines.Where(s => s.Length > 0)
            .Select(s => evaluator(s.Replace(" ", "")))
            .Sum();
        }

        public static void Main(string[] args)
        {
            var lines = File.ReadAllLines(args[0]);
            Console.WriteLine(Run(lines, Rules1));
            Console.WriteLine(Run(lines, Rules2));
        }
    }

    public class Exp1Visitor : Exp1BaseVisitor<long>
    {
        public override long VisitExpression(Exp1Parser.ExpressionContext context)
        {
            var children = context.children;
            if (children.Count == 3)
            {
                var leftChild = children[0];
                if (leftChild == context.LPAREN())
                {
                    return children[1].Accept(this);
                }
                else {
                    var left = children[0].Accept(this);
                    var right = children[2].Accept(this);
                    var op = children[1].GetText();
                    if (op == "+")
                    {
                        return left + right;
                    }
                    else if (op == "*")
                    {
                        return left * right;
                    }
                    else throw new Exception($"Invalid operation {op}");
                }
            }
            else
            {
                return long.Parse(context.GetText());
            }
        }
    }

    public class Exp2Visitor : Exp2BaseVisitor<long>
    {
        public override long VisitMulExp(Exp2Parser.MulExpContext context)
        {
            return context.children.Where((x, i) => i % 2 == 0).Select(x => x.Accept(this)).Aggregate((a, b) => a * b);
        }

        public override long VisitAddExp(Exp2Parser.AddExpContext context)
        {
            return context.children.Where((x, i) => i % 2 == 0).Select(x => x.Accept(this)).Sum();
        }

        public override long VisitParensExp(Exp2Parser.ParensExpContext context)
        {
            return context.children[1].Accept(this);
        }

        public override long VisitNumberExp(Exp2Parser.NumberExpContext context)
        {
            return long.Parse(context.GetText());
        }
    }
}