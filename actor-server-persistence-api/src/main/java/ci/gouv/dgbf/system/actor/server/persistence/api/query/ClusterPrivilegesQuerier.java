package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryHelper;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.query.QueryName;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ClusterPrivileges;

public interface ClusterPrivilegesQuerier extends Querier.CodableAndNamable<ClusterPrivileges> {

	String QUERY_IDENTIFIER_COUNT_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(ClusterPrivileges.class, QueryName.COUNT_WHERE_FILTER.getValue());
	Long countWhereFilter(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI = QueryIdentifierBuilder.getInstance().build(ClusterPrivileges.class, "readWhereFilterForUI");
	Collection<ClusterPrivileges> readWhereFilterForUI(QueryExecutorArguments arguments);
		
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<ClusterPrivileges> implements ClusterPrivilegesQuerier,Serializable {			
		@Override
		public ClusterPrivileges readOne(QueryExecutorArguments arguments) {
			return super.readOne(arguments);
		}
		
		@Override
		public Collection<ClusterPrivileges> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readWhereFilterForUI(arguments);
			return super.readMany(arguments);
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return countWhereFilter(arguments);
			return super.count(arguments);
		}
		
		/**/
		
		@Override
		public Long countWhereFilter(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = new QueryExecutorArguments();
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_WHERE_FILTER);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		@Override
		public Collection<ClusterPrivileges> readWhereFilterForUI(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = new QueryExecutorArguments();
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI);
			Collection<ClusterPrivileges> collection = QueryExecutor.getInstance().executeReadMany(ClusterPrivileges.class, arguments);
			return collection;
		}
		
		/**/
		
		@Override
		protected Class<ClusterPrivileges> getKlass() {
			return ClusterPrivileges.class;
		}
	}
	
	/**/
	
	static ClusterPrivilegesQuerier getInstance() {
		return Helper.getInstance(ClusterPrivilegesQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	/**/
	
	static void initialize() {
		QueryHelper.addQueries(
				Query.buildSelect(ClusterPrivileges.class, QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI, "SELECT t FROM ClusterPrivileges t ORDER BY t.code ASC")
				,Query.buildCount(QUERY_IDENTIFIER_COUNT_WHERE_FILTER, "SELECT COUNT(t.identifier) FROM ClusterPrivileges t")
		);
	}
}