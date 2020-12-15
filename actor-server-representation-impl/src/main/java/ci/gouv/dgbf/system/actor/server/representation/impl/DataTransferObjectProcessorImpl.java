package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import org.cyk.utility.__kernel__.representation.Arguments;
import org.cyk.utility.__kernel__.representation.DataTransferObjectProcessor;
import org.cyk.utility.server.representation.impl.ApplicationProgrammingInterface;

import ci.gouv.dgbf.system.actor.server.representation.api.RequestRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.RequestDto;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class DataTransferObjectProcessorImpl extends DataTransferObjectProcessor.AbstractImpl implements Serializable {

	@Override
	protected <T> void __processRead__(Class<T> klass, Arguments arguments, T dto) {
		super.__processRead__(klass, arguments, dto);
		if(RequestDto.class.equals(klass)) {
			((RequestDto)dto).setReportUniformResourceIdentifier(ApplicationProgrammingInterface.buildResourceIdentifier(RequestRepresentation.PATH
						, RequestRepresentation.PATH_BUILD_REPORT_BY_IDENTIFIER));	
		}
	}
}