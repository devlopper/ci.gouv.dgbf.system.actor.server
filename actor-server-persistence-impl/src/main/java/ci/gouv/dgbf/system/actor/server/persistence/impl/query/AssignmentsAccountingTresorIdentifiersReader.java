package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountingScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;

public class AssignmentsAccountingTresorIdentifiersReader extends AbstractAssignmentsReaderImpl implements Serializable {

	@Override
	protected String getQueryValue() {
		QueryStringBuilder.Arguments arguments = new QueryStringBuilder.Arguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",Assignments.FIELD_IDENTIFIER).addFromTuple("c",AccountingScopeFunction.FIELD_TRESOR_IDENTIFIER);
		arguments.getTuple(Boolean.TRUE).add("Assignments t").addJoins("LEFT JOIN AccountingScopeFunction c ON c.identifier = t.accountingHolder.identifier");	
		arguments.getPredicate(Boolean.TRUE).add("t.identifier IN :"+Querier.PARAMETER_NAME_IDENTIFIERS);
		return QueryStringBuilder.getInstance().build(arguments);
	}

	@Override
	protected void __set__(Assignments assignments, Object[] array) {
		int i = 1;
		assignments.setAccountingTresorIdentifier(getAsString(array, i++));
	}
}