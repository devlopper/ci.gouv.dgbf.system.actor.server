package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.persistence.server.hibernate.AbstractAuditsRecordsByRevisionsNumbersNativeReader;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder.Arguments;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;

public class AssignmentsAuditsRecordsReader extends AbstractAuditsRecordsByRevisionsNumbersNativeReader<Assignments> implements Serializable {
	
	@Override
	protected void __set__(Assignments assignments, Object[] array, Integer i) {
		for(String scopeFunctionFieldName : Assignments.FIELDS_SCOPE_FUNCTIONS_HOLDERS_AS_STRING)
			FieldHelper.write(assignments, scopeFunctionFieldName, getAsString(array, i++));
	}

	@Override
	protected void setProjections(Arguments arguments) {
		super.setProjections(arguments);
		for(String scopeFunctionColumnName : Assignments.COLUMNS_SCOPE_FUNCTIONS_HOLDERS)
			arguments.getProjection(Boolean.TRUE).add(String.format("%1$s.code AS %1$s",scopeFunctionColumnName));
	}
	
	@Override
	protected void setTuple(Arguments arguments) {
		super.setTuple(arguments);
		for(String scopeFunctionColumnName : Assignments.COLUMNS_SCOPE_FUNCTIONS_HOLDERS)
			arguments.getTuple(Boolean.TRUE).addJoins(String.format("LEFT JOIN POSTE %1$s ON %1$s.identifiant = t.%1$s",scopeFunctionColumnName));
	}
	
	@Override
	protected Class<Assignments> getEntityClass() {
		return Assignments.class;
	}
}