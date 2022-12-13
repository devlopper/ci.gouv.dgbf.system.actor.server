package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

public class ReqSecAsCodAdmtvUnitTypStatusCrtionDatPrsngDatAsStrgsReader extends RequestSectionAdministrativeUnitTypeStatusCreationDateProcessingDateAsStringsReader implements Serializable {

	@Override
	protected String getSectionProjection() {
		return "s.code";
	}
}