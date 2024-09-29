# 利用clang前端让basilisk扩展到C++调研

>本调研旨在评估basilisk的词法语法的分析过程。将其与clang的词法语法分析代码进行对比，从而对basilisk的迁移进行评估。

<div id="gitalk-container"></div>
<!-- Gitalk 评论 start  -->

<!-- Link Gitalk 的支持文件  -->
<link rel="stylesheet" href="https://unpkg.com/gitalk/dist/gitalk.css">
<script src="https://unpkg.com/gitalk@latest/dist/gitalk.min.js"></script> 
<div id="gitalk-container"></div>     <script type="text/javascript">
    var gitalk = new Gitalk({

    // gitalk的主要参数
		clientID: `Github Application clientID`,
		clientSecret: `Github Application clientSecret`,
		repo: `GitHub repo`, // 存储你评论 issue 的 Github 仓库名（建议直接用 GitHub Page 的仓库名）
		owner: 'GitHub repo owner', // Github 用户名
		admin: ['GitHub repo owner and collaborators, only these guys can initialize github issues'], // 这个仓库的管理员，可以有多个，用数组表示，一般写自己,里面一定要有创建者登陆github用的名字，如果填错了，评论框会报错
		id: ' location.pathname', // 页面的唯一标识，gitalk 会根据这个标识自动创建的issue的标签 一般使用页面的相对路径作为标识（要填能区分页面唯一性的字符串，比如页面链接，但是要注意长度限制，可以用md5转换一下。如果是单页应用，需要每个路由下能区分页面唯一性的字符串，并且在路由切换的时候重新设置此id）
    	distractionFreeMode: false  // 类似Facebook评论框的全屏遮罩效果
    });
    gitalk.render('gitalk-container');
</script> 
<!-- Gitalk end -->
