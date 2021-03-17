package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.server.query.RuntimeQueryBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class RuntimeQueryBuilderImpl extends RuntimeQueryBuilder.AbstractImpl implements Serializable {

	@Override
	protected Query instantiateQuery(QueryExecutorArguments queryExecutorArguments) {
		Query query = super.instantiateQuery(queryExecutorArguments);
		if(query.getIdentifier().equals(AssignmentsQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_USING_IDENTIFIERS_ONLY))
			query.setTupleFieldsNamesIndexesFromFieldsNames(AssignmentsQueryStringReadWhereFilterBuilder.getTupleFieldsNamesIndexesFromFieldsNames());
		return query;
	}
}