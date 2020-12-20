package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.Map;

import org.cyk.utility.__kernel__.representation.Arguments;
import org.cyk.utility.__kernel__.representation.DataTransferObjectProcessor;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.report.ReportRepresentation;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.representation.entities.RequestDto;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class DataTransferObjectProcessorImpl extends DataTransferObjectProcessor.AbstractImpl implements Serializable {

	@Override
	protected <T> void __processRead__(Class<T> klass, Arguments arguments, T dto) {
		super.__processRead__(klass, arguments, dto);
		if(RequestDto.class.equals(klass)) {
			if(((RequestDto)dto).getType() != null && StringHelper.isNotBlank(((RequestDto)dto).getType().getReportIdentifier()))
				((RequestDto)dto).setReadReportURIQuery(ReportRepresentation.buildURIQuery(((RequestDto)dto).getType().getReportIdentifier()
						, Map.of(Request.COLUMN_IDENTIFIER, ((RequestDto)dto).getIdentifier()), null, null));
		}
	}
}