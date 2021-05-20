package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.server.query.RuntimeQueryBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.LocalityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Locality;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class RuntimeQueryBuilderImpl extends RuntimeQueryBuilder.AbstractImpl implements Serializable {

	@Override
	protected Query instantiateQuery(QueryExecutorArguments queryExecutorArguments) {
		Query query = super.instantiateQuery(queryExecutorArguments);
		if(query.getIdentifier().equals(AssignmentsQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_USING_IDENTIFIERS_ONLY))
			query.setTupleFieldsNamesIndexesFromFieldsNames(AssignmentsQueryStringReadWhereFilterBuilder.getTupleFieldsNamesIndexesFromFieldsNames());
		return query;
	}
	
	@Override
	protected Boolean isBuildable(QueryExecutorArguments arguments) {
		if(arguments != null && arguments.getQuery() != null && AssignmentsQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_USING_IDENTIFIERS_ONLY.equals(arguments.getQuery().getIdentifier()))
			return Boolean.TRUE;
		if(arguments != null && arguments.getQuery() != null && AssignmentsQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER_USING_IDENTIFIERS_ONLY.equals(arguments.getQuery().getIdentifier()))
			return Boolean.TRUE;
		return super.isBuildable(arguments);
	}
	
	@Override
	protected void processQueryExecutorArguments(QueryExecutorArguments arguments) {
		super.processQueryExecutorArguments(arguments);
		if(arguments.getQuery().isIdentifierEqualsDynamic(Locality.class)) {
			if(arguments.getFilter() != null && arguments.getFilter().hasFieldWithPath(LocalityQuerier.PARAMETER_NAME_TYPE) 
					&& arguments.getFilter().getField(LocalityQuerier.PARAMETER_NAME_TYPE).getValue() instanceof String) {
				String type = (String) arguments.getFilter().getField(LocalityQuerier.PARAMETER_NAME_TYPE).getValue();
				if(StringHelper.isNotBlank(type))
					arguments.getFilter().getField(LocalityQuerier.PARAMETER_NAME_TYPE).setValue(Locality.Type.valueOf(type));
			}
		}
	}
}