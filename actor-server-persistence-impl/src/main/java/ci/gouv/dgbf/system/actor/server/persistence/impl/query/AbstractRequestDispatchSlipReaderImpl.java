package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import org.cyk.utility.persistence.server.query.ArraysReaderByIdentifiers;

import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestDispatchSlip;

public abstract class AbstractRequestDispatchSlipReaderImpl extends ArraysReaderByIdentifiers.AbstractImpl.DefaultImpl<RequestDispatchSlip> {

	@Override
	protected Class<RequestDispatchSlip> getEntityClass() {
		return RequestDispatchSlip.class;
	}
}