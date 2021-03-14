package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.server.query.string.RuntimeQueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class RuntimeQueryStringBuilderImpl extends RuntimeQueryStringBuilder.AbstractImpl implements Serializable {

	@Override
	protected String __build__(QueryExecutorArguments arguments) {
		if(AssignmentsQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_USING_IDENTIFIERS_ONLY.equals(arguments.getQuery().getIdentifier()))
			return AssignmentsQueryStringReadWhereFilterBuilder.getRead(arguments);
		if(AssignmentsQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER_USING_IDENTIFIERS_ONLY.equals(arguments.getQuery().getIdentifier()))
			return AssignmentsQueryStringReadWhereFilterBuilder.getCount(arguments);
		return super.__build__(arguments);
	}
}