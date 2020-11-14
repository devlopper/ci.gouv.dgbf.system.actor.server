package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.QueryName;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Cluster;

public interface ClusterQuerier extends Querier.CodableAndNamable<Cluster> {

	String QUERY_IDENTIFIER_READ_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Cluster.class, QueryName.READ_WHERE_FILTER.getValue());
	Collection<Cluster> readWhereFilter(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_COUNT_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Cluster.class, QueryName.COUNT_WHERE_FILTER.getValue());
	Long countWhereFilter(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI = QueryIdentifierBuilder.getInstance().build(Cluster.class, "readWhereFilterForUI");
	Collection<Cluster> readWhereFilterForUI(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_WITH_ALL_BY_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance().build(Cluster.class, "readWithAllByIdentifierForUI");
	Cluster readWithAllByIdentifierForUI(String identifier);
		
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<Cluster> implements ClusterQuerier,Serializable {			
		@Override
		public Cluster readOne(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_WITH_ALL_BY_IDENTIFIER_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readWithAllByIdentifierForUI((String) arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
			return super.readOne(arguments);
		}
		
		@Override
		public Collection<Cluster> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return readWhereFilter(arguments);
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
		public Collection<Cluster> readWhereFilter(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = new QueryExecutorArguments();
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER);
			return QueryExecutor.getInstance().executeReadMany(Cluster.class, arguments);
		}
		
		@Override
		public Long countWhereFilter(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = new QueryExecutorArguments();
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_WHERE_FILTER);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		@Override
		public Collection<Cluster> readWhereFilterForUI(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = new QueryExecutorArguments();
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER);
			Collection<Cluster> clusters = readWhereFilter(arguments);
			return clusters;
		}
		
		@Override
		public Cluster readWithAllByIdentifierForUI(String identifier) {
			Cluster cluster = EntityFinder.getInstance().find(Cluster.class, identifier);
			return cluster;
		}
		
		/**/
		
		@Override
		protected Class<Cluster> getKlass() {
			return Cluster.class;
		}
	}
	
	/**/
	
	static ClusterQuerier getInstance() {
		return Helper.getInstance(ClusterQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	/**/
	
	static void initialize() {
		QueryHelper.addQueries(
				Query.buildSelect(Cluster.class, QUERY_IDENTIFIER_READ_WHERE_FILTER, "SELECT t FROM Cluster t ORDER BY t.code ASC")
				,Query.buildCount(QUERY_IDENTIFIER_COUNT_WHERE_FILTER, "SELECT COUNT(t.identifier) FROM Cluster t")
		);
	}
}