package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import org.cyk.utility.__kernel__.representation.Arguments;
import org.cyk.utility.__kernel__.representation.DataTransferObjectProcessor;

import ci.gouv.dgbf.system.actor.server.representation.entities.RequestDto;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class DataTransferObjectProcessorImpl extends DataTransferObjectProcessor.AbstractImpl implements Serializable {

	@Override
	protected <T> void __processRead__(Class<T> klass, Arguments arguments, T dto) {
		super.__processRead__(klass, arguments, dto);
		if(RequestDto.class.equals(klass)) {
			((RequestDto)dto).setReadReportURIQuery("identifier=/reports/sigobe/acteur/FicheGestCredit&parametersNamesValuesAsJson=%7B\"identifiant\"%3A\""
					+((RequestDto)dto).getIdentifier()+"\"%7D");
		}
	}
}