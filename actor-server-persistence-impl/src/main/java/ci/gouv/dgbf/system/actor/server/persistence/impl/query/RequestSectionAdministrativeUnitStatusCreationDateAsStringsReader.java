package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;
import java.time.LocalDateTime;

import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.time.TimeHelper;
import org.cyk.utility.persistence.query.Language;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestStatus;

public class RequestSectionAdministrativeUnitStatusCreationDateAsStringsReader extends AbstractRequestReaderImpl implements Serializable {

	@Override
	protected QueryStringBuilder.Arguments instantiateQueryStringBuilderArguments() {
		QueryStringBuilder.Arguments arguments =  super.instantiateQueryStringBuilderArguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",Request.FIELD_IDENTIFIER,FieldHelper.join(Request.FIELD_STATUS,RequestStatus.FIELD_NAME)
				,Request.FIELD_CREATION_DATE);
		arguments.getProjection(Boolean.TRUE).add(Language.Select.concatCodeName("s"),Language.Select.concatCodeName("au"));
		arguments.getTuple(Boolean.TRUE).addJoins("LEFT JOIN AdministrativeUnit au ON au = t.administrativeUnit").addJoins("LEFT JOIN Section s ON s = au.section");
		return arguments;
	}
	
	@Override
	protected void __set__(Request request, Object[] array) {
		Integer index = 1;
		request.setStatusAsString(getAsString(array, index++));
		request.setCreationDateAsString(formatLocalDateTime((LocalDateTime)array[index++]));
		request.setSectionAsString(getAsString(array, index++));
		request.setAdministrativeUnitAsString(getAsString(array, index++));
		__set__(request, array, index);
	}
	
	protected void __set__(Request request, Object[] array,Integer index) {}
	
	protected String formatLocalDateTime(LocalDateTime dateTime) {
		return TimeHelper.formatLocalDateTime(dateTime,"dd/MM/yyyy à HH:mm");
	}
}