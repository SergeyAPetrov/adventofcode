using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using QuickGraph;

namespace cshaprPlayground
{
    class Program
    {
        static void Main(string[] args)
        {
            var graph = new UndirectedGraph<int, Edge<int>>();
            
            graph.AddVerticesAndEdgeRange(new[] { new Edge<int>(1, 2) });
            graph.AddVertex(1);
        }
    }
}
