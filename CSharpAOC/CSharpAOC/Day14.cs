using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace CSharpAOC
{
    public static class Day14
    {
        public static int Part2()
        {
            using (MD5 md5Hash = MD5.Create())
            {
                int currState = 0;
                string salt = "ihaygndm";
                List<int> found = new List<int>();
                while (found.Count < 64)
                {
                    string s = First3InARow(GetCachedMD5(md5Hash, salt, currState));
                    if (s != null)
                    {
                        IEnumerable<int> poss = Enumerable.Range(currState + 1, 1000);
                        if (poss.Any(i => Match5InARow(s, GetCachedMD5(md5Hash, salt, i))))
                        {
                            found.Add(currState);
                        }
                    }
                    currState += 1;
                }
                return found[63];
            }
        }

        static readonly Dictionary<int, string> MD5Cache = new Dictionary<int, string>();

        static string GetCachedMD5(MD5 md5Hash, string salt, int currState)
        {
            if (!MD5Cache.ContainsKey(currState))
            {
                string hash = GetMd5Hash(md5Hash, salt + currState);
                foreach (int i in Enumerable.Range(0, 2016))
                {
                    hash = GetMd5Hash(md5Hash, hash);
                }
                MD5Cache.Add(currState, hash);
            }

            return MD5Cache[currState];
        }

        static bool Match5InARow(string m, string s)
        {
            return Regex.IsMatch(s, ".*" + m + "{5}.*");
        }
        static string First3InARow(string s)
        {
            var matches = Regex.Match(s, @".*?(.)\1{2}.*");
            return matches.Success
                ? matches.Groups[1].Value
                : null;
        }
        static string GetMd5Hash(MD5 md5Hash, string input)
        {
            byte[] data = md5Hash.ComputeHash(Encoding.UTF8.GetBytes(input));
            StringBuilder sBuilder = new StringBuilder();
            for (int i = 0; i < data.Length; i++)
            {
                sBuilder.Append(data[i].ToString("x2"));
            }
            return sBuilder.ToString();
        }
    }
}
