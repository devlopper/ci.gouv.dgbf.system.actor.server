package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.server.query.ArraysReaderByIdentifiers;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public class AssignmentsHoldersReader extends ArraysReaderByIdentifiers.AbstractImpl.DefaultImpl<Assignments> {

	@Override
	protected String getQueryValue() {
		QueryStringBuilder.Arguments arguments = new QueryStringBuilder.Arguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",Assignments.FIELD_IDENTIFIER);
		arguments.getTuple(Boolean.TRUE).add("Assignments t");
		for(String fieldName : Assignments.FIELDS_SCOPES_FUNCTIONS_HOLDERS) {
			arguments.getProjection().addFromTuple(fieldName,ScopeFunction.FIELD_IDENTIFIER,ScopeFunction.FIELD_CODE,ScopeFunction.FIELD_NAME);
			arguments.getTuple().addJoins(String.format("LEFT JOIN ScopeFunction %1$s ON %1$s = t.%1$s", fieldName));
		}
		arguments.getPredicate(Boolean.TRUE).add("t.identifier IN :"+Querier.PARAMETER_NAME_IDENTIFIERS);
		return QueryStringBuilder.getInstance().build(arguments);
	}
	
	@Override
	protected void __set__(Assignments assignments, Object[] array) {
		int i = 1;
		if(StringHelper.isNotBlank((String)array[i]))
			assignments.setCreditManagerHolder(new ScopeFunction().setIdentifier((String)array[i++]).setCode((String)array[i++]).setName((String)array[i++]));
		else
			i += 3;
		
		if(StringHelper.isNotBlank((String)array[i]))
			assignments.setAuthorizingOfficerHolder(new ScopeFunction().setIdentifier((String)array[i++]).setCode((String)array[i++]).setName((String)array[i++]));
		else
			i += 3;
		
		if(StringHelper.isNotBlank((String)array[i]))
			assignments.setFinancialControllerHolder(new ScopeFunction().setIdentifier((String)array[i++]).setCode((String)array[i++]).setName((String)array[i++]));
		else
			i += 3;
		
		if(StringHelper.isNotBlank((String)array[i]))
			assignments.setAccountingHolder(new ScopeFunction().setIdentifier((String)array[i++]).setCode((String)array[i++]).setName((String)array[i++]));
		else
			i += 3;
	}
	
	@Override
	protected Class<Assignments> getEntityClass() {
		return Assignments.class;
	}
}