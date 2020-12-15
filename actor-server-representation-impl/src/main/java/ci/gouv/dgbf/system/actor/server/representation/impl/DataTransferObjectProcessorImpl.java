package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import org.cyk.utility.__kernel__.enumeration.Action;
import org.cyk.utility.__kernel__.representation.DataTransferObjectProcessor;
import org.cyk.utility.server.representation.impl.ApplicationProgrammingInterface;

import ci.gouv.dgbf.system.actor.server.representation.api.RequestRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.RequestDto;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class DataTransferObjectProcessorImpl extends DataTransferObjectProcessor.AbstractImpl implements Serializable {

	@Override
	protected <T> void __process__(Class<T> klass, Action action, T dto) {
		super.__process__(klass, action, dto);
		if(RequestDto.class.equals(klass)) {
			if(Action.READ.equals(action)) {
				((RequestDto)dto).setReportUniformResourceIdentifier(ApplicationProgrammingInterface.buildResourceIdentifier(RequestRepresentation.PATH
						, RequestRepresentation.PATH_BUILD_REPORT_BY_IDENTIFIER));
			}		
		}
	}	
}